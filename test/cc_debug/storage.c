_Thread_local int global_tls = 84;
static int global_static = 64;

int main(int argc, char **argv) {
    int vla[argc + 3];
    static int local_static = 74;
    static _Thread_local int local_tls = 94;
    vla[0] = 14;
    vla[1] = 28;
    // storage-break
    return vla[0] + vla[1] + local_static + local_tls + global_tls + global_static != 358;
}
