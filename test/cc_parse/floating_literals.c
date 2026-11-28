// Check exact decimal rounding and rounding in the format of the suffix.
static_assert(3.875 == 0x1.fp1);
static_assert(9007199254740993.0 == 0x1p53);
static_assert(9007199254740995.0 == 0x1.0000000000002p53);
static_assert(1.000000059604644775390625f == 1.0f);
static_assert(1.000000059604644775390626f == 0x1.000002p0f);
static_assert(1.00048828125f16 == 1.0f16);
static_assert(1.000488281251f16 == 0x1.004p0f16);
static_assert(4.9406564584124654e-324 == 0x1p-1074);
static_assert(0x1p-1075 == 0.0);

// These values exceed the range or precision of the f64 AST projection.
long double largest = 1.18973149535723176502126385303097021e4932L;
long double smallest = 3.64519953188247460252840593361941982e-4951L;
long double next = 0x1.0000000000000002p0L;
