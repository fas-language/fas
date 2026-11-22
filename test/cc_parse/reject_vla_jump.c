int f(int n) {
    goto next;
    int a[n] = {};
next:
    return sizeof a;
}
