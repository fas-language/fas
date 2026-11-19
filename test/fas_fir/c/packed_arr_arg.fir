; abi_fuzz campaign-9 find 160591, reduced (see packed_arr_arg.c)
triple "x86_64-unknown-linux-gnu"
global @abi_sink size 8 align 8 global = extern

declare @c_callee_0 (agg(10, 1, [0: i8, 1: agg(3, 1, [0: i8, 1: i16]), 4: agg(3, 1, [0: i8, 1: i16]), 7: agg(3, 1, [0: i8, 1: i16])])) -> () global
func @fir_callee_0 (agg(10, 1, [0: i8, 1: agg(3, 1, [0: i8, 1: i16]), 4: agg(3, 1, [0: i8, 1: i16]), 7: agg(3, 1, [0: i8, 1: i16])])) -> () global {
^e(%a0: ptr):
  %t1 = alloca ptr i64 16, align 16
  %t2 = alloca ptr i64 24, align 8
  %t3 = ptradd ptr %a0, i64 0, 1, 0
  %t4 = load i8 %t3, align 1
  %t5 = zext i64 %t4
  %t6 = xor i64 i64 -3750763034362895579, %t5
  %t7 = mul i64 %t6, i64 1099511628211
  %t8 = ptradd ptr %a0, i64 0, 1, 1
  %t9 = load i8 %t8, align 1
  %t10 = sext i64 %t9
  %t11 = xor i64 %t7, %t10
  %t12 = mul i64 %t11, i64 1099511628211
  %t13 = ptradd ptr %a0, i64 0, 1, 2
  %t14 = load i16 %t13, align 1
  %t15 = zext i64 %t14
  %t16 = xor i64 %t12, %t15
  %t17 = mul i64 %t16, i64 1099511628211
  %t18 = ptradd ptr %a0, i64 0, 1, 4
  %t19 = load i8 %t18, align 1
  %t20 = sext i64 %t19
  %t21 = xor i64 %t17, %t20
  %t22 = mul i64 %t21, i64 1099511628211
  %t23 = ptradd ptr %a0, i64 0, 1, 5
  %t24 = load i16 %t23, align 1
  %t25 = zext i64 %t24
  %t26 = xor i64 %t22, %t25
  %t27 = mul i64 %t26, i64 1099511628211
  %t28 = ptradd ptr %a0, i64 0, 1, 7
  %t29 = load i8 %t28, align 1
  %t30 = sext i64 %t29
  %t31 = xor i64 %t27, %t30
  %t32 = mul i64 %t31, i64 1099511628211
  %t33 = ptradd ptr %a0, i64 0, 1, 8
  %t34 = load i16 %t33, align 1
  %t35 = zext i64 %t34
  %t36 = xor i64 %t32, %t35
  %t37 = mul i64 %t36, i64 1099511628211
  store %t37, ptr @abi_sink, align 8
  ret
}

func @f2c_0 () -> (i64) global {
^e:
  %t38 = alloca ptr i64 16, align 16
  %t39 = alloca ptr i64 10, align 16
  memset %t39, i8 0, i64 10
  %t40 = trunc i8 i64 -9223372036854775808
  %t41 = ptradd ptr %t39, i64 0, 1, 0
  store %t40, %t41, align 1
  %t42 = trunc i8 i64 -8570443552569469017
  %t43 = ptradd ptr %t39, i64 0, 1, 1
  store %t42, %t43, align 1
  %t44 = trunc i16 i64 2735259375553021124
  %t45 = ptradd ptr %t39, i64 0, 1, 2
  store %t44, %t45, align 1
  %t46 = trunc i8 i64 2652153946013077005
  %t47 = ptradd ptr %t39, i64 0, 1, 4
  store %t46, %t47, align 1
  %t48 = trunc i16 i64 -1
  %t49 = ptradd ptr %t39, i64 0, 1, 5
  store %t48, %t49, align 1
  %t50 = trunc i8 i64 128
  %t51 = ptradd ptr %t39, i64 0, 1, 7
  store %t50, %t51, align 1
  %t52 = trunc i16 i64 32768
  %t53 = ptradd ptr %t39, i64 0, 1, 8
  store %t52, %t53, align 1
  store i64 0, ptr @abi_sink, align 8
  call (agg(10, 1, [0: i8, 1: agg(3, 1, [0: i8, 1: i16]), 4: agg(3, 1, [0: i8, 1: i16]), 7: agg(3, 1, [0: i8, 1: i16])])) -> () @c_callee_0(%t39)
  %t54 = load i64 ptr @abi_sink, align 8
  ret %t54
}

func @f2f_0 () -> (i64) global {
^e:
  %t55 = alloca ptr i64 16, align 16
  %t56 = alloca ptr i64 10, align 16
  memset %t56, i8 0, i64 10
  %t57 = trunc i8 i64 -9223372036854775808
  %t58 = ptradd ptr %t56, i64 0, 1, 0
  store %t57, %t58, align 1
  %t59 = trunc i8 i64 -8570443552569469017
  %t60 = ptradd ptr %t56, i64 0, 1, 1
  store %t59, %t60, align 1
  %t61 = trunc i16 i64 2735259375553021124
  %t62 = ptradd ptr %t56, i64 0, 1, 2
  store %t61, %t62, align 1
  %t63 = trunc i8 i64 2652153946013077005
  %t64 = ptradd ptr %t56, i64 0, 1, 4
  store %t63, %t64, align 1
  %t65 = trunc i16 i64 -1
  %t66 = ptradd ptr %t56, i64 0, 1, 5
  store %t65, %t66, align 1
  %t67 = trunc i8 i64 128
  %t68 = ptradd ptr %t56, i64 0, 1, 7
  store %t67, %t68, align 1
  %t69 = trunc i16 i64 32768
  %t70 = ptradd ptr %t56, i64 0, 1, 8
  store %t69, %t70, align 1
  store i64 0, ptr @abi_sink, align 8
  call (agg(10, 1, [0: i8, 1: agg(3, 1, [0: i8, 1: i16]), 4: agg(3, 1, [0: i8, 1: i16]), 7: agg(3, 1, [0: i8, 1: i16])])) -> () @fir_callee_0(%t56)
  %t71 = load i64 ptr @abi_sink, align 8
  ret %t71
}

