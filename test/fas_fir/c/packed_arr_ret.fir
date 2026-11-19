; abi_fuzz campaign-9 find 694009, reduced (see packed_arr_ret.c)
triple "x86_64-unknown-linux-gnu"
global @abi_sink size 8 align 8 global = extern

declare @c_callee_0 () -> (agg(14, 1, [0: f16, 2: agg(6, 1, [0: i16, 2: i32]), 8: agg(6, 1, [0: i16, 2: i32])])) global
func @fir_callee_0 () -> (agg(14, 1, [0: f16, 2: agg(6, 1, [0: i16, 2: i32]), 8: agg(6, 1, [0: i16, 2: i32])])) global {
^e():
  %t1 = alloca ptr i64 16, align 16
  %t2 = alloca ptr i64 24, align 8
  %t3 = alloca ptr i64 14, align 16
  %t4 = mul i64 i64 -3750763034362895579, i64 3
  %t5 = add i64 %t4, i64 0
  %t6 = mul i64 i64 -3750763034362895579, i64 5
  %t7 = add i64 %t6, i64 -7046029254386353131
  %t8 = mul i64 i64 -3750763034362895579, i64 7
  %t9 = add i64 %t8, i64 4354685564936845354
  %t10 = mul i64 i64 -3750763034362895579, i64 9
  %t11 = add i64 %t10, i64 -2691343689449507777
  %t12 = mul i64 i64 -3750763034362895579, i64 11
  %t13 = add i64 %t12, i64 8709371129873690708
  memset %t3, i8 0, i64 14
  %t15 = trunc i8 %t5
  %t14 = sitofp f16 %t15
  %t16 = ptradd ptr %t3, i64 0, 1, 0
  store %t14, %t16, align 1
  %t17 = trunc i16 %t7
  %t18 = ptradd ptr %t3, i64 0, 1, 2
  store %t17, %t18, align 1
  %t19 = trunc i32 %t9
  %t20 = ptradd ptr %t3, i64 0, 1, 4
  store %t19, %t20, align 1
  %t21 = trunc i16 %t11
  %t22 = ptradd ptr %t3, i64 0, 1, 8
  store %t21, %t22, align 1
  %t23 = trunc i32 %t13
  %t24 = ptradd ptr %t3, i64 0, 1, 10
  store %t23, %t24, align 1
  ret %t3
}

func @f2c_0 () -> (i64) global {
^e:
  %t25 = alloca ptr i64 16, align 16
  %t26 = call () -> (agg(14, 1, [0: f16, 2: agg(6, 1, [0: i16, 2: i32]), 8: agg(6, 1, [0: i16, 2: i32])])) @c_callee_0()
  %t27 = ptradd ptr %t26, i64 0, 1, 0
  %t28 = load f16 %t27, align 1
  %t29 = bitcast i16 %t28
  %t30 = zext i64 %t29
  %t31 = xor i64 i64 -3750763034362895579, %t30
  %t32 = mul i64 %t31, i64 1099511628211
  %t33 = ptradd ptr %t26, i64 0, 1, 2
  %t34 = load i16 %t33, align 1
  %t35 = zext i64 %t34
  %t36 = xor i64 %t32, %t35
  %t37 = mul i64 %t36, i64 1099511628211
  %t38 = ptradd ptr %t26, i64 0, 1, 4
  %t39 = load i32 %t38, align 1
  %t40 = sext i64 %t39
  %t41 = xor i64 %t37, %t40
  %t42 = mul i64 %t41, i64 1099511628211
  %t43 = ptradd ptr %t26, i64 0, 1, 8
  %t44 = load i16 %t43, align 1
  %t45 = zext i64 %t44
  %t46 = xor i64 %t42, %t45
  %t47 = mul i64 %t46, i64 1099511628211
  %t48 = ptradd ptr %t26, i64 0, 1, 10
  %t49 = load i32 %t48, align 1
  %t50 = sext i64 %t49
  %t51 = xor i64 %t47, %t50
  %t52 = mul i64 %t51, i64 1099511628211
  ret %t52
}

func @f2f_0 () -> (i64) global {
^e:
  %t53 = alloca ptr i64 16, align 16
  %t54 = call () -> (agg(14, 1, [0: f16, 2: agg(6, 1, [0: i16, 2: i32]), 8: agg(6, 1, [0: i16, 2: i32])])) @fir_callee_0()
  %t55 = ptradd ptr %t54, i64 0, 1, 0
  %t56 = load f16 %t55, align 1
  %t57 = bitcast i16 %t56
  %t58 = zext i64 %t57
  %t59 = xor i64 i64 -3750763034362895579, %t58
  %t60 = mul i64 %t59, i64 1099511628211
  %t61 = ptradd ptr %t54, i64 0, 1, 2
  %t62 = load i16 %t61, align 1
  %t63 = zext i64 %t62
  %t64 = xor i64 %t60, %t63
  %t65 = mul i64 %t64, i64 1099511628211
  %t66 = ptradd ptr %t54, i64 0, 1, 4
  %t67 = load i32 %t66, align 1
  %t68 = sext i64 %t67
  %t69 = xor i64 %t65, %t68
  %t70 = mul i64 %t69, i64 1099511628211
  %t71 = ptradd ptr %t54, i64 0, 1, 8
  %t72 = load i16 %t71, align 1
  %t73 = zext i64 %t72
  %t74 = xor i64 %t70, %t73
  %t75 = mul i64 %t74, i64 1099511628211
  %t76 = ptradd ptr %t54, i64 0, 1, 10
  %t77 = load i32 %t76, align 1
  %t78 = sext i64 %t77
  %t79 = xor i64 %t75, %t78
  %t80 = mul i64 %t79, i64 1099511628211
  ret %t80
}

