/* C side of test/errors_export.fas: calls the exported `fails` functions
   through the header `fas --emit-header` writes for them. */
#include <stdio.h>
#include <string.h>
#include "errors_export.h"

int c_check(void) {
    int bad = 0;
    ex_i32_result a = ex_i32(21);
    bad |= a.err != 0 || a.value != 42;
    a = ex_i32(-1);
    bad |= (a.err != FAS_ERROR_NotFound || FAS_ERROR_NotFound != 2) << 1;
    ex_u8_result b = ex_u8(3);
    bad |= (b.err != 0 || b.value != 203) << 2;
    b = ex_u8(-3);
    bad |= (b.err != FAS_ERROR_Bad) << 3;
    ex_i64_result c = ex_i64(2);
    bad |= (c.err != 0 || c.value != -6000000000LL) << 4;
    ex_f64_result d = ex_f64(1);
    bad |= (d.err != 0 || d.value != 1.25) << 5;
    ex_f32_result e = ex_f32(1);
    bad |= (e.err != 0 || e.value != 2.5f) << 6;
    ex_ptr_result f = ex_ptr(1);
    bad |= (f.err != 0 || strcmp(f.value, "fas") != 0) << 7;
    ex_bool_result g = ex_bool(9);
    bad |= (g.err != 0 || !g.value) << 8;
    g = ex_bool(-9);
    bad |= (g.err != FAS_ERROR_Bad) << 9;
    bad |= (ex_void(1) != 0) << 10;
    bad |= (ex_void(-1) != FAS_ERROR_Overflow) << 11;
    ex_err_result h = ex_err(1);
    bad |= (h.err != 0 || h.value != FAS_ERROR_NotFound) << 12;
    ex_mode_result m = ex_mode(1);
    bad |= (m.err != 0 || m.value != 2) << 13;
    ex_pair_result p = ex_pair(5);
    bad |= (p.err != 0 || p.value.a != 5 || p.value.b != 6) << 14;
    p = ex_pair(-5);
    bad |= (p.err != FAS_ERROR_Bad) << 15;
    ex_big_result q = ex_big(7000000000LL);
    bad |= (q.err != 0 || q.value.a != 7000000000LL || q.value.b != -7000000000LL) << 16;
    q = ex_big(-1);
    bad |= (q.err != FAS_ERROR_Bad) << 17;
    printf("export bad %d\n", bad);
    return bad;
}
