/* Test-only Linux interposer. Publication faults affect only fas-ld temporaries;
   read faults affect only the explicitly selected input, without changing it.
   Neither affects stderr, the parent process, or another invocation. */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

static ssize_t (*real_write)(int, const void *, size_t);
static ssize_t (*real_read)(int, void *, size_t);
static int (*real_open)(const char *, int, ...);
static int (*real_open64)(const char *, int, ...);
static int (*real_close)(int);
static int (*real_fchmod)(int, mode_t);
static int (*real_rename)(const char *, const char *);
static const char *fault;
static int ordinal = 1, writes, chmods, reads, creates;
static struct { dev_t device; ino_t inode; } files[8];
static int file_count;

__attribute__((constructor)) static void initialize(void) {
    real_write = dlsym(RTLD_NEXT, "write");
    real_read = dlsym(RTLD_NEXT, "read");
    real_open = dlsym(RTLD_NEXT, "open");
    real_open64 = dlsym(RTLD_NEXT, "open64");
    real_close = dlsym(RTLD_NEXT, "close");
    real_fchmod = dlsym(RTLD_NEXT, "fchmod");
    real_rename = dlsym(RTLD_NEXT, "rename");
    fault = getenv("FAS_IO_FAULT");
    const char *number = getenv("FAS_IO_ORDINAL");
    if (number) ordinal = atoi(number);
    if (!real_write || !real_read || !real_open || !real_open64 ||
        !real_close || !real_fchmod || !real_rename) _exit(125);
}

static int temporary(const char *path) {
    const char *base = strrchr(path, '/');
    base = base ? base + 1 : path;
    size_t length = strlen(base);
    return !strncmp(base, ".fas-ld-", 8) && length >= 12 &&
           !strcmp(base + length - 4, ".tmp");
}

static int selected(struct stat *st) {
    for (int n = 0; n < file_count; ++n)
        if (files[n].device == st->st_dev && files[n].inode == st->st_ino)
            return n + 1 == ordinal;
    if (file_count == 8) _exit(124);
    files[file_count].device = st->st_dev;
    files[file_count].inode = st->st_ino;
    return ++file_count == ordinal;
}

static int target(int fd) {
    if (!fault) return 0;
    int saved = errno;
    char name[64], path[65537];
    snprintf(name, sizeof name, "/proc/self/fd/%d", fd);
    ssize_t length = readlink(name, path, sizeof path - 1);
    struct stat st;
    int result = 0;
    if (length >= 0) {
        path[length] = 0;
        if (temporary(path) && !fstat(fd, &st)) result = selected(&st);
    }
    errno = saved;
    return result;
}

static int input(int fd) {
    const char *selected_path = getenv("FAS_IO_INPUT");
    if (!fault || !selected_path || !*selected_path) return 0;
    int saved = errno;
    char name[64], path[65537];
    snprintf(name, sizeof name, "/proc/self/fd/%d", fd);
    ssize_t length = readlink(name, path, sizeof path - 1);
    int result = 0;
    if (length >= 0) {
        path[length] = 0;
        result = !strcmp(path, selected_path);
    }
    errno = saved;
    return result;
}

ssize_t read(int fd, void *data, size_t size) {
    if (input(fd)) {
        ++reads;
        if (!strcmp(fault, "read-eio")) { errno = EIO; return -1; }
        if (!strcmp(fault, "read-eof")) return 0;
        if (!strcmp(fault, "read-eintr") && reads == 1) { errno = EINTR; return -1; }
        if (!strcmp(fault, "read-short") && size > 17) size = 17;
    }
    return real_read(fd, data, size);
}

static int create_fault(const char *path, int flags) {
    if (!fault || !(flags & O_CREAT) || !temporary(path)) return 0;
    int number = ++creates;
    if (!strcmp(fault, "create-collision") && number == ordinal) return EEXIST;
    /* Filename.open_temp_file retries Sys_error with another random name.
       Persistent resource failures must survive those retries. */
    if (number < ordinal) return 0;
    if (!strcmp(fault, "create-emfile")) return EMFILE;
    if (!strcmp(fault, "create-enospc")) return ENOSPC;
    if (!strcmp(fault, "create-eacces")) return EACCES;
    return 0;
}

#define OPEN_WRAPPER(symbol) \
int symbol(const char *path, int flags, ...) { \
    mode_t mode = 0; \
    if ((flags & O_CREAT) || (flags & O_TMPFILE) == O_TMPFILE) { \
        va_list args; va_start(args, flags); mode = va_arg(args, mode_t); va_end(args); \
    } \
    int error = create_fault(path, flags); \
    if (error) { errno = error; return -1; } \
    return real_##symbol(path, flags, mode); \
}

OPEN_WRAPPER(open)
OPEN_WRAPPER(open64)

ssize_t write(int fd, const void *data, size_t size) {
    if (target(fd)) {
        ++writes;
        if (!strcmp(fault, "write-enospc")) { errno = ENOSPC; return -1; }
        if (!strcmp(fault, "write-eintr") && writes == 1) { errno = EINTR; return -1; }
        if (!strcmp(fault, "write-partial-eio") && writes > 1) { errno = EIO; return -1; }
        if (!strcmp(fault, "write-short") || !strcmp(fault, "write-partial-eio") ||
            !strcmp(fault, "stop-during-write")) {
            if (size > 17) size = 17;
            ssize_t result = real_write(fd, data, size);
            if (!strcmp(fault, "stop-during-write")) {
                const char *ready = getenv("FAS_IO_READY_FD");
                if (!ready || real_write(atoi(ready), "R", 1) != 1) _exit(123);
                raise(SIGSTOP);
            }
            return result;
        }
    }
    return real_write(fd, data, size);
}

int close(int fd) {
    if (target(fd) && !strcmp(fault, "close-eio")) {
        int result = real_close(fd);
        if (result) return result;
        errno = EIO;
        return -1;
    }
    return real_close(fd);
}

int fchmod(int fd, mode_t mode) {
    if (target(fd) && ++chmods == 2 && !strcmp(fault, "chmod-eperm")) {
        errno = EPERM;
        return -1;
    }
    return real_fchmod(fd, mode);
}

int rename(const char *old, const char *new) {
    if (fault && temporary(old) && !strcmp(fault, "rename-eio")) {
        struct stat st;
        if (!stat(old, &st) && selected(&st)) { errno = EIO; return -1; }
    }
    return real_rename(old, new);
}
