# Single-steps a whole run from main and unwinds at every instruction with
# CFI alone (gdb's prologue analyzers, which would unwind rbp frames without
# it, are disabled): every frame chain must end at main.
import gdb
gdb.execute("set pagination off")
gdb.execute("set debuginfod enabled off")
gdb.execute("break main")
gdb.execute("run")
gdb.execute("maint frame-unwinder disable -class ARCH")
bad = 0
steps = 0
seen = set()
while steps < 100000:
    if gdb.selected_inferior().pid == 0:
        break  # the process exited
    pc = int(gdb.parse_and_eval("$pc").cast(gdb.lookup_type("long")))
    names = []
    try:
        f = gdb.newest_frame()
        while f is not None:
            names.append(f.name() or hex(f.pc()))
            f = f.older()
    except gdb.error as e:
        names.append("<%s>" % e)
    seen.add(names[0])
    if names[-1] != "main" and names[0] != "_start":
        bad += 1
        if bad <= 10:
            print("BAD %#x %s" % (pc, " <- ".join(names)))
    steps += 1
    try:
        gdb.execute("stepi", to_string=True)
    except gdb.error:
        break
print("steps %d bad %d" % (steps, bad))
print("seen " + " ".join(sorted(seen)))
