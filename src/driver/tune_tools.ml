let runner_source =
  {c|#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <linux/perf_event.h>
#include <sched.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

static int counter(pid_t pid, uint64_t config) {
    struct perf_event_attr a;
    memset(&a, 0, sizeof a);
    a.type = PERF_TYPE_HARDWARE;
    a.size = sizeof a;
    a.config = config;
    a.disabled = 1;
    a.enable_on_exec = 1;
    a.inherit = 1;
    a.exclude_kernel = 1;
    a.exclude_hv = 1;
    return (int)syscall(SYS_perf_event_open, &a, pid, -1, -1, PERF_FLAG_FD_CLOEXEC);
}

static void value(char *out, size_t size, int fd) {
    uint64_t v;
    if (fd < 0 || read(fd, &v, sizeof v) != (ssize_t)sizeof v) snprintf(out, size, "-");
    else snprintf(out, size, "%llu", (unsigned long long)v);
}

static int open_out(const char *dir, const char *name) {
    char path[4096];
    snprintf(path, sizeof path, "%s/%s", dir, name);
    return open(path, O_WRONLY | O_CREAT | O_TRUNC | O_CLOEXEC, 0644);
}

static uint64_t now_ns(void) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return (uint64_t)t.tv_sec * 1000000000u + (uint64_t)t.tv_nsec;
}

int main(int argc, char **argv) {
    if (argc < 8 || strcmp(argv[6], "--") != 0) {
        fprintf(stderr, "usage: runner CPU ARENA OFFSET OUTDIR PATH -- ARGV0 [ARGS...]\n");
        return 2;
    }
    int cpu = atoi(argv[1]);
    const char *arena = argv[2], *offset = argv[3], *dir = argv[4], *path = argv[5];
    int out = open_out(dir, "stdout"), err = open_out(dir, "stderr");
    int null = open("/dev/null", O_RDONLY | O_CLOEXEC);
    if (out < 0 || err < 0 || null < 0) {
        perror("runner");
        return 2;
    }
    int gate[2];
    if (pipe2(gate, O_CLOEXEC) != 0) {
        perror("runner");
        return 2;
    }
    pid_t pid = fork();
    if (pid < 0) {
        perror("runner");
        return 2;
    }
    if (pid == 0) {
        char go;
        close(gate[1]);
        if (read(gate[0], &go, 1) != 1) _exit(127);
        if (cpu >= 0) {
            cpu_set_t set;
            CPU_ZERO(&set);
            CPU_SET(cpu, &set);
            if (sched_setaffinity(0, sizeof set, &set) != 0) {
                dprintf(err, "runner: cannot pin to CPU %d: %s\n", cpu, strerror(errno));
                _exit(127);
            }
        }
        if (strcmp(arena, "-") != 0) {
            setenv("LD_PRELOAD", arena, 1);
            setenv("FAS_ARENA_OFFSET", offset, 1);
        }
        dup2(null, 0);
        dup2(out, 1);
        dup2(err, 2);
        execv(path, argv + 7);
        dprintf(2, "runner: cannot run %s: %s\n", path, strerror(errno));
        _exit(127);
    }
    close(gate[0]);
    int cycles = counter(pid, PERF_COUNT_HW_CPU_CYCLES);
    int instructions = counter(pid, PERF_COUNT_HW_INSTRUCTIONS);
    uint64_t start = now_ns();
    if (write(gate[1], "g", 1) != 1) {
        perror("runner");
        kill(pid, SIGKILL);
    }
    close(gate[1]);
    int status;
    struct rusage usage;
    while (wait4(pid, &status, 0, &usage) < 0) {
        if (errno != EINTR) {
            perror("runner");
            return 2;
        }
    }
    uint64_t wall = now_ns() - start;
    char c[32], i[32], ended[32];
    value(c, sizeof c, cycles);
    value(i, sizeof i, instructions);
    if (WIFEXITED(status)) snprintf(ended, sizeof ended, "%d", WEXITSTATUS(status));
    else {
        const char *name = sigabbrev_np(WTERMSIG(status));
        if (name) snprintf(ended, sizeof ended, "sig:SIG%s", name);
        else snprintf(ended, sizeof ended, "sig:%d", WTERMSIG(status));
    }
    int result = open_out(dir, "result");
    if (result < 0) {
        perror("runner");
        return 2;
    }
    dprintf(result, "%llu %s %s %s %ld\n", (unsigned long long)wall, c, i, ended, usage.ru_maxrss);
    close(result);
    return 0;
}
|c}

let arena_source =
  {c|#define _GNU_SOURCE
#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#define HUGE ((size_t)2 << 20)
#define CLASSES 48

static char *base, *cur, *end;
static void *lists[CLASSES];
static int lock;
static int failed;

static void take(void) {
    while (__atomic_exchange_n(&lock, 1, __ATOMIC_ACQUIRE))
        while (__atomic_load_n(&lock, __ATOMIC_RELAXED)) __builtin_ia32_pause();
}

static void drop(void) { __atomic_store_n(&lock, 0, __ATOMIC_RELEASE); }

static int init(void) {
    if (failed) return 0;
    size_t arena = (size_t)64 << 30;
    char *p = MAP_FAILED;
    while (arena >= ((size_t)64 << 20)) {
        p = mmap(0, arena + HUGE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
        if (p != MAP_FAILED) break;
        arena >>= 1;
    }
    if (p == MAP_FAILED) {
        failed = 1;
        return 0;
    }
    base = (char *)(((uintptr_t)p + HUGE - 1) & ~(uintptr_t)(HUGE - 1));
    end = base + arena;
    madvise(p, arena + HUGE, MADV_DONTDUMP);
    madvise(base, arena, MADV_HUGEPAGE);
    cur = base;
    const char *o = getenv("FAS_ARENA_OFFSET");
    if (o) cur += (strtoull(o, 0, 10) & ~(unsigned long long)63) % HUGE;
    return 1;
}

static int class_of(size_t n) {
    int c = 6;
    while (c < CLASSES && ((size_t)1 << c) < n) c++;
    return c;
}

static void *block(int c) {
    if (lists[c]) {
        void *b = lists[c];
        lists[c] = *(void **)b;
        return b;
    }
    if (!cur && !init()) return 0;
    size_t sz = (size_t)1 << c;
    size_t al = sz < 4096 ? sz : 4096;
    char *b = (char *)(((uintptr_t)cur + al - 1) & ~(uintptr_t)(al - 1));
    if (b > end || sz > (size_t)(end - b)) return 0;
    cur = b + sz;
    return b;
}

static void *place(size_t n, size_t align) {
    if (align < 16) align = 16;
    if (align > HUGE || n > ((size_t)1 << (CLASSES - 2))) return 0;
    int c = class_of(n + align + 16);
    if (c >= CLASSES) return 0;
    take();
    char *b = block(c);
    drop();
    if (!b) return 0;
    char *p = (char *)(((uintptr_t)b + 16 + align - 1) & ~(uintptr_t)(align - 1));
    ((size_t *)p)[-2] = (size_t)c;
    ((void **)p)[-1] = b;
    return p;
}

static size_t usable(void *p) {
    size_t c = ((size_t *)p)[-2];
    char *b = ((void **)p)[-1];
    return ((size_t)1 << c) - (size_t)((char *)p - b);
}

static void *aligned(size_t align, size_t n) {
    if (!align || (align & (align - 1))) {
        errno = EINVAL;
        return 0;
    }
    void *p = place(n, align);
    if (!p) errno = ENOMEM;
    return p;
}

void *malloc(size_t n) {
    void *p = place(n, 16);
    if (!p) errno = ENOMEM;
    return p;
}

void free(void *p) {
    if (!p) return;
    int c = (int)((size_t *)p)[-2];
    void *b = ((void **)p)[-1];
    take();
    *(void **)b = lists[c];
    lists[c] = b;
    drop();
}

void *calloc(size_t a, size_t b) {
    if (b && a > (size_t)-1 / b) {
        errno = ENOMEM;
        return 0;
    }
    void *p = malloc(a * b);
    if (p) memset(p, 0, a * b);
    return p;
}

void *realloc(void *o, size_t n) {
    if (!o) return malloc(n);
    if (!n) {
        free(o);
        return 0;
    }
    size_t have = usable(o);
    if (n <= have) return o;
    void *p = malloc(n);
    if (!p) return 0;
    memcpy(p, o, have);
    free(o);
    return p;
}

void *reallocarray(void *o, size_t a, size_t b) {
    if (b && a > (size_t)-1 / b) {
        errno = ENOMEM;
        return 0;
    }
    return realloc(o, a * b);
}

void *aligned_alloc(size_t align, size_t n) { return aligned(align, n); }
void *memalign(size_t align, size_t n) { return aligned(align, n); }

int posix_memalign(void **r, size_t align, size_t n) {
    if (align < sizeof(void *) || (align & (align - 1))) return EINVAL;
    void *p = place(n, align);
    if (!p) return ENOMEM;
    *r = p;
    return 0;
}

void *valloc(size_t n) { return aligned(4096, n); }

void *pvalloc(size_t n) {
    size_t page = 4096;
    if (n > (size_t)-1 - page) {
        errno = ENOMEM;
        return 0;
    }
    return aligned(page, (n + page - 1) & ~(page - 1));
}

size_t malloc_usable_size(void *p) { return p ? usable(p) : 0; }
|c}

let isolate_source =
  {sh|set -u
mode=$0
cpu=${1:-}
self=${2:-0}
root=${3:-}
sys=$root/sys
proc=$root/proc
top=$sys/devices/system/cpu
state=$root/run/fas-tune-isolate
affinity=$root/run/fas-tune-isolate.affinity
case $cpu in
''|*[!0-9]*) echo "fas-tune-isolate: CPU must be a number" >&2; exit 2 ;;
esac
[ -r "$top/cpu$cpu/topology/thread_siblings_list" ] || { echo "fas-tune-isolate: no CPU $cpu" >&2; exit 2; }
expand() { echo "$1" | tr ',' '\n' | awk -F- 'NF { b = (NF > 1) ? $2 : $1; for (c = $1; c <= b; c++) print c }'; }
join() { paste -sd, -; }
mask() { expand "$1" | awk 'BEGIN { n = 0 } { w = int($1 / 32); m[w] += 2 ^ ($1 % 32); if (w > n) n = w } END { s = sprintf("%x", m[n]); for (w = n - 1; w >= 0; w--) s = s sprintf(",%08x", m[w]); print s }'; }
core=$(expand "$(cat "$top/cpu$cpu/topology/thread_siblings_list")")
sib=$(echo "$core" | join)
last=$(ls -d "$top"/cpu[0-9]* | sed 's/.*cpu//' | sort -n | tail -n 1)
all=0-$last
house=$(seq 0 "$last" | grep -v -x -F "$core" | join)
[ -n "$house" ] || { echo "fas-tune-isolate: no CPU left for other work" >&2; exit 2; }
user=${SUDO_USER:-$(id -un)}
skip=$(sed -n 's|^0::.*/\([^/]*\.scope\)$|\1|p' "$proc/$self/cgroup" 2>/dev/null)
units() {
    echo system.slice
    echo init.scope
    systemctl list-units --type=scope --no-legend --plain 'session-*.scope' 2>/dev/null | awk '{ print $1 }' | grep -v -x -F "${skip:-/}"
}
started() { s=$(ps -o lstart= -p "$1" 2>/dev/null | tr -s ' ' '_'); echo "${s:--}"; }
current() { taskset -pc "$1" 2>/dev/null | sed 's/.*: //'; }
save() { [ -e "$1" ] && printf 'file\t%s\t%s\n' "$1" "$(cat "$1")" >>"$state"; }
put() { [ -w "$1" ] && echo "$2" >"$1" 2>/dev/null; }
on() {
    if [ ! -e "$state" ]; then
        echo "# cpu $cpu pid $self" >"$state" || exit 1
        for c in $core; do
            save "$top/cpu$c/cpufreq/scaling_governor"
            for d in "$top/cpu$c"/cpuidle/state*/disable; do save "$d"; done
        done
        save "$proc/sys/kernel/nmi_watchdog"
        save "$sys/devices/virtual/workqueue/cpumask"
        save "$proc/irq/default_smp_affinity"
        for f in "$proc"/irq/*/smp_affinity_list; do save "$f"; done
        for u in $(units); do
            printf 'unit\t%s\t%s\n' "$u" "$(systemctl show -p AllowedCPUs --value "$u" 2>/dev/null)" >>"$state"
        done
    fi
    for c in $core; do
        put "$top/cpu$c/cpufreq/scaling_governor" performance
        for d in "$top/cpu$c"/cpuidle/state*; do
            [ -r "$d/latency" ] && [ "$(cat "$d/latency")" -gt 1 ] && put "$d/disable" 1
        done
    done
    put "$proc/sys/kernel/nmi_watchdog" 0
    for u in $(units); do systemctl set-property --runtime "$u" AllowedCPUs="$house"; done
    for f in "$proc"/irq/*/smp_affinity_list; do put "$f" "$house"; done
    put "$proc/irq/default_smp_affinity" "$(mask "$house")"
    put "$sys/devices/virtual/workqueue/cpumask" "$(mask "$house")"
    [ -e "$affinity" ] || : >"$affinity"
    for p in $(pgrep -u "$user"); do
        cur=$(current "$p")
        [ -n "$cur" ] || continue
        grep -q "^$p " "$affinity" || printf '%s %s %s\n' "$p" "$(started "$p")" "$cur" >>"$affinity"
        new=$(expand "$cur" | grep -v -x -F "$core" | join)
        [ -n "$new" ] || continue
        [ "$new" != "$(expand "$cur" | join)" ] || continue
        taskset -a -cp "$new" "$p" >/dev/null 2>&1
    done
    echo "isolated $sib"
}
off() {
    restored=
    if [ -e "$affinity" ]; then
        while read -r p start m; do
            [ "$(started "$p")" = "$start" ] || continue
            restored="$restored $p "
            taskset -a -cp "$m" "$p" >/dev/null 2>&1
        done <"$affinity"
    fi
    hs=$(expand "$house" | join)
    for p in $(pgrep -u "$user"); do
        case $restored in *" $p "*) continue ;; esac
        cur=$(current "$p")
        [ -n "$cur" ] || continue
        [ "$(expand "$cur" | join)" = "$hs" ] && taskset -a -cp "$all" "$p" >/dev/null 2>&1
    done
    rm -f "$affinity"
    if [ -e "$state" ]; then
        tab=$(printf '\t')
        while IFS=$tab read -r kind name value; do
            case $kind in
            file) put "$name" "$value" ;;
            unit) systemctl set-property --runtime "$name" AllowedCPUs="$value" ;;
            esac
        done <"$state"
        rm -f "$state"
    fi
    echo "released $sib"
}
case $mode in
on) on ;;
off) off ;;
hold)
    trap '' INT TERM HUP
    on
    cat >/dev/null
    off
    ;;
*) echo "usage: sh -c SCRIPT on|off|hold CPU [PID [ROOT]]" >&2; exit 2 ;;
esac
exit 0
|sh}

let ( let* ) = Result.bind

let build ~cc dir =
  let write name text =
    let path = Filename.concat dir name in
    Out_channel.with_open_bin path (fun channel -> output_string channel text);
    path
  in
  let compile flags source output =
    match Process.run (Array.of_list ((cc :: flags) @ [ source; "-o"; output ])) with
    | Ok _ -> Ok output
    | Error (failure : Process.failure) ->
        Error
          (Printf.sprintf "cannot build the tune %s with %s: %s"
             (Filename.chop_extension (Filename.basename source))
             cc
             (String.trim failure.Process.stderr))
  in
  try
    ignore (write "isolate.sh" isolate_source);
    let* runner =
      compile [ "-O2" ] (write "runner.c" runner_source) (Filename.concat dir "runner")
    in
    let* arena =
      compile [ "-O2"; "-shared"; "-fPIC" ]
        (write "arena.c" arena_source)
        (Filename.concat dir "arena.so")
    in
    Ok (runner, arena)
  with Sys_error message -> Error message
