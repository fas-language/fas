#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <locale.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
typedef int locale_t;
#define LC_NUMERIC_MASK 0
#define newlocale(mask, name, base) 1
#define strtof_l(text, end, locale) __mingw_strtof(text, end)
#define strtod_l(text, end, locale) __mingw_strtod(text, end)
#define roundevenf nearbyintf
#define roundeven nearbyint

typedef unsigned __int128 fas_u128;

static void fas_split(double x, fas_u128 *mantissa, int *exponent) {
  uint64_t bits;
  memcpy(&bits, &x, sizeof(bits));
  int biased = (int)((bits >> 52) & 0x7ff);
  uint64_t fraction = bits & ((1ULL << 52) - 1);
  *mantissa = biased == 0 ? fraction : (fraction | (1ULL << 52));
  *exponent = biased == 0 ? -1074 : biased - 1075;
}

static int fas_top_bit(fas_u128 value) {
  uint64_t high = (uint64_t)(value >> 64);
  return high ? 127 - __builtin_clzll(high) : 63 - __builtin_clzll((uint64_t)value);
}

static void fas_normalize(fas_u128 *mantissa, int *exponent) {
  int move = 125 - fas_top_bit(*mantissa);
  *mantissa <<= move;
  *exponent -= move;
}

static uint64_t fas_exact_fma(double x, double y, double z, int precision, int min_exp,
                              int max_biased) {
  fas_u128 product, addend;
  int product_exp, addend_exp, y_exp;
  fas_u128 y_mantissa;
  fas_split(x, &product, &product_exp);
  fas_split(y, &y_mantissa, &y_exp);
  product *= y_mantissa;
  product_exp += y_exp;
  fas_split(z, &addend, &addend_exp);
  int product_sign = !signbit(x) != !signbit(y);
  int addend_sign = signbit(z) != 0;
  fas_normalize(&product, &product_exp);
  fas_normalize(&addend, &addend_exp);
  fas_u128 big = product, small = addend;
  int big_exp = product_exp, small_exp = addend_exp, sign = product_sign;
  if (addend_exp > product_exp || (addend_exp == product_exp && addend > product)) {
    big = addend;
    small = product;
    big_exp = addend_exp;
    small_exp = product_exp;
    sign = addend_sign;
  }
  int distance = big_exp - small_exp;
  if (distance >= 128)
    small = 1;
  else if (distance > 0) {
    fas_u128 lost = small & ((((fas_u128)1) << distance) - 1);
    small = (small >> distance) | (lost != 0);
  }
  fas_u128 sum = product_sign == addend_sign ? big + small : big - small;
  if (sum == 0) return 0;
  int top = fas_top_bit(sum);
  int shift = top + big_exp >= min_exp ? top - (precision - 1)
                                       : (min_exp - (precision - 1)) - big_exp;
  uint64_t sign_bit = (uint64_t)sign << (precision == 53 ? 63 : 31);
  if (shift > top + 1) return sign_bit;
  fas_u128 kept = shift <= 0 ? sum << -shift : sum >> shift;
  if (shift > 0) {
    fas_u128 rest = sum & ((((fas_u128)1) << shift) - 1);
    fas_u128 half = ((fas_u128)1) << (shift - 1);
    if (rest > half || (rest == half && (kept & 1))) kept += 1;
  }
  int low_exp = shift + big_exp;
  uint64_t significand = (uint64_t)kept;
  uint64_t hidden = 1ULL << (precision - 1);
  if (significand == hidden << 1) {
    significand = hidden;
    low_exp += 1;
  }
  if (significand < hidden) return sign_bit | significand;
  int biased = low_exp + (precision - 1) - (min_exp - 1);
  if (biased >= max_biased) return sign_bit | ((uint64_t)max_biased << (precision - 1));
  return sign_bit | ((uint64_t)biased << (precision - 1)) | (significand - hidden);
}

static double fas_fma(double x, double y, double z) {
  if (!isfinite(x) || !isfinite(y)) return x * y + z;
  if (!isfinite(z)) return z;
  if (x == 0 || y == 0) return x * y + z;
  if (z == 0) return x * y;
  uint64_t bits = fas_exact_fma(x, y, z, 53, -1022, 2047);
  double result;
  memcpy(&result, &bits, sizeof(result));
  return result;
}

static float fas_fmaf(float x, float y, float z) {
  if (!isfinite(x) || !isfinite(y)) return x * y + z;
  if (!isfinite(z)) return z;
  if (x == 0 || y == 0) return x * y + z;
  if (z == 0) return x * y;
  uint32_t bits = (uint32_t)fas_exact_fma(x, y, z, 24, -126, 255);
  float result;
  memcpy(&result, &bits, sizeof(result));
  return result;
}

#define fma fas_fma
#define fmaf fas_fmaf
#endif

static locale_t fas_c_locale;

static locale_t get_c_locale(void) {
  if (fas_c_locale == (locale_t)0) {
    fas_c_locale = newlocale(LC_NUMERIC_MASK, "C", (locale_t)0);
    if (fas_c_locale == (locale_t)0) caml_failwith("cannot create C numeric locale");
  }
  return fas_c_locale;
}

CAMLprim value fas_parse_f32_bits(value text) {
  float parsed = strtof_l(String_val(text), NULL, get_c_locale());
  uint32_t bits;
  memcpy(&bits, &parsed, sizeof(bits));
  return caml_copy_int64((int64_t)bits);
}

CAMLprim value fas_parse_f64_bits(value text) {
  double parsed = strtod_l(String_val(text), NULL, get_c_locale());
  uint64_t bits;
  memcpy(&bits, &parsed, sizeof(bits));
  return caml_copy_int64((int64_t)bits);
}

CAMLprim value fas_float_literal_kind(value text) {
  const char *input = String_val(text); size_t length = strlen(input); char *end;
  if (length == 0 || *input == '+' || *input == '-') return Val_int(0);
  (void)strtod_l(input, &end, get_c_locale()); size_t used = (size_t)(end - input); int kind = 1;
  if (used != length) {
    char suffix = input[length - 1]; if (used + 1 != length || !strchr("fFlL", suffix)) return Val_int(0);
    kind = suffix == 'f' || suffix == 'F' ? 2 : 3;
  }
  int hex = length > 1 && input[0] == '0' && (input[1] == 'x' || input[1] == 'X'); if (hex && !memchr(input, 'p', used) && !memchr(input, 'P', used)) return Val_int(0);
  if (!hex && !memchr(input, '.', used) && !memchr(input, 'e', used) && !memchr(input, 'E', used)) return Val_int(0);
  size_t mantissa_end = used; for (size_t i = hex ? 2 : 0; i < used; i++)
    if (strchr(hex ? "pP" : "eE", input[i])) { mantissa_end = i; break; }
  int nonzero = 0;
  for (size_t i = hex ? 2 : 0; i < mantissa_end; i++)
    nonzero |= ((input[i] >= '1' && input[i] <= '9') || (hex && ((input[i] >= 'a' && input[i] <= 'f') || (input[i] >= 'A' && input[i] <= 'F'))));
  return Val_int(kind | (nonzero << 2));
}

CAMLprim value fas_f32_binary_bits(value operation, value left_bits, value right_bits) {
  uint32_t left_raw = (uint32_t)Int64_val(left_bits);
  uint32_t right_raw = (uint32_t)Int64_val(right_bits);
  float left;
  float right;
  memcpy(&left, &left_raw, sizeof(left));
  memcpy(&right, &right_raw, sizeof(right));
  volatile float result;
  switch (Int_val(operation)) {
  case 0: result = left + right; break;
  case 1: result = left - right; break;
  case 2: result = left * right; break;
  default: result = left / right; break;
  }
  uint32_t result_bits;
  memcpy(&result_bits, (const void *)&result, sizeof(result_bits));
  return caml_copy_int64((int64_t)result_bits);
}

CAMLprim value fas_float_unary_bits(value operation, value is_f32, value input_bits) {
  int op = Int_val(operation);
  if (Bool_val(is_f32)) {
    uint32_t bits = (uint32_t)Int64_val(input_bits);
    if (op == 4) return caml_copy_int64((int64_t)(bits & 0x7fffffffU));
    float input;
    memcpy(&input, &bits, sizeof(input));
    float result;
    switch (op) {
    case 0: result = sqrtf(input); break;
    case 1: result = floorf(input); break;
    case 2: result = ceilf(input); break;
    default: result = roundevenf(input); break;
    }
    memcpy(&bits, &result, sizeof(bits));
    return caml_copy_int64((int64_t)bits);
  }
  uint64_t bits = (uint64_t)Int64_val(input_bits);
  if (op == 4) return caml_copy_int64((int64_t)(bits & 0x7fffffffffffffffULL));
  double input;
  memcpy(&input, &bits, sizeof(input));
  double result;
  switch (op) {
  case 0: result = sqrt(input); break;
  case 1: result = floor(input); break;
  case 2: result = ceil(input); break;
  default: result = roundeven(input); break;
  }
  memcpy(&bits, &result, sizeof(bits));
  return caml_copy_int64((int64_t)bits);
}

CAMLprim value fas_float_fma_bits(value is_f32, value a_bits, value b_bits, value c_bits) {
  if (Bool_val(is_f32)) {
    uint32_t a_raw = (uint32_t)Int64_val(a_bits);
    uint32_t b_raw = (uint32_t)Int64_val(b_bits);
    uint32_t c_raw = (uint32_t)Int64_val(c_bits);
    float a;
    float b;
    float c;
    memcpy(&a, &a_raw, sizeof(a));
    memcpy(&b, &b_raw, sizeof(b));
    memcpy(&c, &c_raw, sizeof(c));
    float result = fmaf(a, b, c);
    uint32_t result_bits;
    memcpy(&result_bits, &result, sizeof(result_bits));
    return caml_copy_int64((int64_t)result_bits);
  }
  uint64_t a_raw = (uint64_t)Int64_val(a_bits);
  uint64_t b_raw = (uint64_t)Int64_val(b_bits);
  uint64_t c_raw = (uint64_t)Int64_val(c_bits);
  double a;
  double b;
  double c;
  memcpy(&a, &a_raw, sizeof(a));
  memcpy(&b, &b_raw, sizeof(b));
  memcpy(&c, &c_raw, sizeof(c));
  double result = fma(a, b, c);
  uint64_t result_bits;
  memcpy(&result_bits, &result, sizeof(result_bits));
  return caml_copy_int64((int64_t)result_bits);
}

CAMLprim value fas_int_to_float_bits(value raw, value width, value is_signed, value to_f32) {
  int64_t signed_value = Int64_val(raw);
  uint64_t unsigned_value = (uint64_t)signed_value;
  unsigned bits = (unsigned)Int_val(width);
  uint64_t result_bits;
  if (Bool_val(is_signed)) {
    switch (bits) {
    case 8: signed_value = (int8_t)signed_value; break;
    case 16: signed_value = (int16_t)signed_value; break;
    case 32: signed_value = (int32_t)signed_value; break;
    }
    if (Bool_val(to_f32)) {
      float result = (float)signed_value;
      uint32_t narrowed;
      memcpy(&narrowed, &result, sizeof(narrowed));
      result_bits = narrowed;
    } else {
      double result = (double)signed_value;
      memcpy(&result_bits, &result, sizeof(result_bits));
    }
  } else {
    if (bits == 8) unsigned_value = (uint8_t)unsigned_value;
    else if (bits == 16) unsigned_value = (uint16_t)unsigned_value;
    else if (bits == 32) unsigned_value = (uint32_t)unsigned_value;
    if (Bool_val(to_f32)) {
      float result = (float)unsigned_value;
      uint32_t narrowed;
      memcpy(&narrowed, &result, sizeof(narrowed));
      result_bits = narrowed;
    } else {
      double result = (double)unsigned_value;
      memcpy(&result_bits, &result, sizeof(result_bits));
    }
  }
  return caml_copy_int64((int64_t)result_bits);
}

CAMLprim value fas_float_to_int_bits(value raw, value format, value width, value is_signed) {
  int64_t input_bits = Int64_val(raw);
  unsigned source_format = (unsigned)Int_val(format);
  unsigned bits = (unsigned)Int_val(width);
  long double input;
  if (source_format == 32) {
    uint32_t narrowed = (uint32_t)input_bits;
    float value;
    memcpy(&value, &narrowed, sizeof(value));
    input = value;
  } else {
    double value;
    memcpy(&value, &input_bits, sizeof(value));
    input = value;
  }
  uint64_t result;
  if (Bool_val(is_signed)) {
    int64_t minimum = bits == 64 ? INT64_MIN : -(INT64_C(1) << (bits - 1));
    int64_t maximum = bits == 64 ? INT64_MAX : (INT64_C(1) << (bits - 1)) - 1;
    if (isnan(input)) result = 0;
    else if (input <= (long double)minimum) result = (uint64_t)minimum;
    else if (input >= (long double)maximum) result = (uint64_t)maximum;
    else result = (uint64_t)(int64_t)input;
  } else {
    uint64_t maximum = bits == 64 ? UINT64_MAX : (UINT64_C(1) << bits) - 1;
    if (isnan(input) || input <= 0.0L) result = 0;
    else if (input >= (long double)maximum) result = maximum;
    else result = (uint64_t)input;
  }
  if (bits < 64) result &= (UINT64_C(1) << bits) - 1;
  int64_t boxed;
  memcpy(&boxed, &result, sizeof(boxed));
  return caml_copy_int64(boxed);
}
