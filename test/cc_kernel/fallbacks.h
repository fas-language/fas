/* Forced into every kernel C file that fcc compiles (see cc.sh).
 *
 * seqlock.h declares __scoped_seqlock_bug extern and never defines it, so
 * the link fails unless the compiler proves its call impossible. That needs
 * inlining __scoped_seqlock_next into the scoped_seqlock_read loop and
 * propagating the loop condition, which fcc does not do yet. The kernel
 * itself uses this empty definition for compilers that cannot prove it
 * (GCC 8 on some targets, KASAN, UBSAN_ALIGNMENT). The later extern
 * declaration keeps this internal linkage.
 *
 * TODO: remove once fcc inlines and propagates branch conditions. */
static inline void __scoped_seqlock_bug(void) { }
