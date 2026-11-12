#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <locale.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

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
