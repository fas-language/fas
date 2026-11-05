/* Test-only Linux interposer. Faults affect only fas-ld publication temporaries,
   never inputs, stderr, the parent process, or another invocation. */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

static ssize_t (*real_write)(int, const void *, size_t);
static int (*real_close)(int);
static int (*real_fchmod)(int, mode_t);
static int (*real_rename)(const char *, const char *);
static const char *fault;
static int ordinal = 1, writes, chmods;
static struct { dev_t device; ino_t inode; } files[8];
static int file_count;

__attribute__((constructor)) static void initialize(void) {
    real_write = dlsym(RTLD_NEXT, "write");
    real_close = dlsym(RTLD_NEXT, "close");
    real_fchmod = dlsym(RTLD_NEXT, "fchmod");
    real_rename = dlsym(RTLD_NEXT, "rename");
    fault = getenv("FAS_IO_FAULT");
    const char *number = getenv("FAS_IO_ORDINAL");
    if (number) ordinal = atoi(number);
    if (!real_write || !real_close || !real_fchmod || !real_rename) _exit(125);
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
