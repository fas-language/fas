#include <stdarg.h>

void consume(va_list ap) {
    (void)va_arg(ap, void);
}
