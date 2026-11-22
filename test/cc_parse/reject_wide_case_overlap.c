int overlap(_BitInt(129) v) {
    switch (v) {
    case (((_BitInt(129))1) << 100) ... (((_BitInt(129))1) << 101): return 1;
    case (((_BitInt(129))1) << 100) + 1: return 2;
    default: return 0;
    }
}
