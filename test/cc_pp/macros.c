#define x 2
#define f(a) f(x * (a))
#define z z[0]
#define h g(~
#define g(a) f(a)
#define m(a) a(w)
#define w 0,1
#define t(a) a
f(y+1) + f(f(z)) % t(t(g)(0) + t)(1);
g(x+(3,4)-(w)) | h 5) & m(f)^m(m);
#define glue(a,b) a ## b
#define stringify(x) #x
glue(he,llo) glue(,x) glue(x,) glue(,)
stringify(a   /* gap */ b "a\\b" 'x')
#define optional(head, ...) head __VA_OPT__(+ __VA_ARGS__)
#define nothing
#define twice(x) x x
twice(__COUNTER__)
#define dupva(...) __VA_OPT__(__VA_ARGS__ __VA_ARGS__)
dupva(__COUNTER__) __COUNTER__
optional(1)
optional(2, nothing)
optional(3, 4)
#define optional_paste(...) left ## __VA_OPT__(right)
optional_paste() optional_paste(yes)
#define fcc_ucn_\u03b1 42
fcc_ucn_α
#define VA_X 9
#define va_string(x,...) #__VA_OPT__(x)
#define va_join(x,...) left ## __VA_OPT__(x)
#define va_inner(x,...) __VA_OPT__(a ## x)
va_string(VA_X, 1) va_string(VA_X) va_join(VA_X, 1) va_inner(VA_X, 1)
#define va_end_marker(x, ...) __VA_OPT__(a x ## x) ## b
va_end_marker(, 1)
