#include <stdio.h>
#include <unwind.h>

static _Unwind_Reason_Code count_frame(struct _Unwind_Context *ctx, void *data)
{
    if (_Unwind_GetIP(ctx))
        ++*(int *)data;
    return _URC_NO_REASON;
}

__attribute__((noinline)) static int trace(void)
{
    int count = 0;
    _Unwind_Backtrace(count_frame, &count);
    return count;
}

__attribute__((noinline)) static int short_trace(void)
{
    asm volatile("nop; .cfi_escape 0x07, %P0; nop; nop" : : "i" (sizeof(long) * 2));
    return trace();
}

int main(void)
{
    // Trace through trace(), main(), and the C runtime startup.
    int reached_caller = trace() >= 4;
    int stopped_early = short_trace() == 2;
    printf("backtrace full and shortened: %d %d\n", reached_caller, stopped_early);
    return !reached_caller || !stopped_early;
}
