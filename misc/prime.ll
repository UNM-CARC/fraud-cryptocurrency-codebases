source_filename = "test"
target datalayout = "e-m:e-p:64:64-i64:64-f80:128-n8:16:32:64-S128"

@rax = internal unnamed_addr global i64 0
@rbp = internal unnamed_addr global i64 0
@global_var_4048 = external global i64
@global_var_2005.2 = constant [27 x i8] c"Enter a positive integer: \00"
@global_var_4060.3 = global i64 0
@global_var_4180.4 = global i64 0
@global_var_2020.5 = constant [23 x i8] c"This is a prime number\00"
@global_var_2037.6 = constant [27 x i8] c"This is not a prime number\00"
@global_var_4299 = external global i64
@global_var_3da8.8 = global i64 4464
@global_var_3db8.9 = global i64 4384
@0 = external global i32
@global_var_4298.1 = local_unnamed_addr global i8 0
@global_var_3ff8.7 = local_unnamed_addr global void (i64*)* null

define i64 @_init() local_unnamed_addr {
dec_label_pc_1000:
  %tmp = load i64, i64* @rax, align 8
  ret i64 %tmp
}

define i64 @function_1003() local_unnamed_addr {
dec_label_pc_1003:
  %v0_1008 = load i64, i64* inttoptr (i64 16360 to i64*), align 8
  %v1_100f = icmp eq i64 %v0_1008, 0
  br i1 %v1_100f, label %dec_label_pc_1016, label %dec_label_pc_1014

dec_label_pc_1014:                                ; preds = %dec_label_pc_1003
  call void @__gmon_start__()
  br label %dec_label_pc_1016

dec_label_pc_1016:                                ; preds = %dec_label_pc_1014, %dec_label_pc_1003
  %v0_101a = phi i64 [ ptrtoint (i32* @0 to i64), %dec_label_pc_1014 ], [ 0, %dec_label_pc_1003 ]
  ret i64 %v0_101a
}

define i64 @function_1030(i64* %arg1, i64* %arg2) local_unnamed_addr {
dec_label_pc_1030:
  %v0_1030 = call i64 @_ZNSirsERi()
  ret i64 %v0_1030
}

define i32 @function_1040(void (i64*)* %func, i64* %arg, i64* %dso_handle) local_unnamed_addr {
dec_label_pc_1040:
  %v6_1040 = call i32 @__cxa_atexit(void (i64*)* %func, i64* %arg, i64* %dso_handle)
  ret i32 %v6_1040
}

define i64 @function_1050(i64* %arg1, i8* %arg2, i64 %arg3) local_unnamed_addr {
dec_label_pc_1050:
  %v0_1050 = call i64 @_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc()
  ret i64 %v0_1050
}

define void @function_1060() local_unnamed_addr {
dec_label_pc_1060:
  call void @__stack_chk_fail()
  ret void
}

define i64 @function_1070(i64 %arg1) local_unnamed_addr {
dec_label_pc_1070:
  %v0_1070 = call i64 @_ZNSt8ios_base4InitC1Ev()
  ret i64 %v0_1070
}

define i64 @_start() local_unnamed_addr {
dec_label_pc_1080:
  %rax.global-to-local = alloca i64, align 8
  %tmp234 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %tmp234
}

define i64 @deregister_tm_clones() local_unnamed_addr {
dec_label_pc_10b0:
  ret i64 16464
}

define i64 @register_tm_clones() local_unnamed_addr {
dec_label_pc_10e0:
  ret i64 0
}

define i64 @__do_global_dtors_aux() local_unnamed_addr {
dec_label_pc_1120:
  %rax.global-to-local = alloca i64, align 8
  %tmp234 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %tmp234
}

define i64 @function_1123() local_unnamed_addr {
dec_label_pc_1123:
  %rax.global-to-local = alloca i64, align 8
  %rbp.global-to-local = alloca i64, align 8
  %stack_var_-8 = alloca i64, align 8
  %v0_1124 = load i8, i8* @global_var_4298.1, align 1
  %v7_1124 = icmp eq i8 %v0_1124, 0
  %v1_112b = icmp eq i1 %v7_1124, false
  br i1 %v1_112b, label %dec_label_pc_1160, label %dec_label_pc_112d

dec_label_pc_112d:                                ; preds = %dec_label_pc_1123
  %v0_112d = load i64, i64* %rbp.global-to-local, align 8
  store i64 %v0_112d, i64* %stack_var_-8, align 8
  %v4_112d = ptrtoint i64* %stack_var_-8 to i64
  %v0_112e = load i64, i64* inttoptr (i64 16336 to i64*), align 16
  %v7_112e = icmp eq i64 %v0_112e, 0
  store i64 %v4_112d, i64* %rbp.global-to-local, align 8
  br i1 %v7_112e, label %dec_label_pc_1148, label %dec_label_pc_113b

dec_label_pc_113b:                                ; preds = %dec_label_pc_112d
  %v0_113b = load i64, i64* inttoptr (i64 16456 to i64*), align 8
  %v1_1142 = inttoptr i64 %v0_113b to i64*
  call void @__cxa_finalize(i64* %v1_1142)
  store i64 ptrtoint (i32* @0 to i64), i64* %rax.global-to-local, align 8
  br label %dec_label_pc_1148

dec_label_pc_1148:                                ; preds = %dec_label_pc_113b, %dec_label_pc_112d
  %v0_1148 = call i64 @deregister_tm_clones()
  store i64 %v0_1148, i64* %rax.global-to-local, align 8
  store i8 1, i8* @global_var_4298.1, align 1
  %v2_1154 = load i64, i64* %stack_var_-8, align 8
  store i64 %v2_1154, i64* %rbp.global-to-local, align 8
  ret i64 %v0_1148

dec_label_pc_1160:                                ; preds = %dec_label_pc_1123
  %v0_1160 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %v0_1160
}

define i64 @frame_dummy() local_unnamed_addr {
dec_label_pc_1170:
  %rax.global-to-local = alloca i64, align 8
  %tmp234 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %tmp234
}

define i64 @function_1173() local_unnamed_addr {
dec_label_pc_1173:
  %v0_1174 = call i64 @register_tm_clones()
  ret i64 %v0_1174
}

define i64 @main(i64 %argc, i8** %argv) local_unnamed_addr {
dec_label_pc_1179:
  %rax.global-to-local = alloca i64, align 8
  %rdx.global-to-local = alloca i64, align 8
  %stack_var_-24 = alloca i32, align 4
  %v0_1181 = call i64 @__readfsqword(i64 40)
  store i64 0, i64* %rax.global-to-local, align 8
  %v5_11a2 = load i64, i64* %rdx.global-to-local, align 8
  %v6_11a2 = call i64 @function_1050(i64* nonnull @global_var_4060.3, i8* getelementptr inbounds ([27 x i8], [27 x i8]* @global_var_2005.2, i64 0, i64 0), i64 %v5_11a2)
  %v2_11a7 = ptrtoint i32* %stack_var_-24 to i64
  store i64 %v2_11a7, i64* %rax.global-to-local, align 8
  %v3_11b5 = bitcast i32* %stack_var_-24 to i64*
  %v4_11b5 = call i64 @function_1030(i64* nonnull @global_var_4180.4, i64* %v3_11b5)
  store i64 %v4_11b5, i64* %rax.global-to-local, align 8
  %v3_11c1 = load i32, i32* %stack_var_-24, align 4
  %tmp272 = icmp slt i32 %v3_11c1, 0
  %v3_11c6 = zext i1 %tmp272 to i32
  %v10_11c6 = zext i1 %tmp272 to i64
  %v4_11d2 = sext i32 %v3_11c1 to i64
  %v2_11d5 = ashr i32 %v3_11c1, 31
  %v3_11d5 = zext i32 %v2_11d5 to i64
  %v6_11d6 = mul nuw i64 %v3_11d5, 4294967296
  %v7_11d6 = or i64 %v6_11d6, %v4_11d2
  %v4_11c9 = add i32 %v3_11c6, %v3_11c1
  %v3_11cb = sdiv i32 %v4_11c9, 2
  %v10_11cb = zext i32 %v3_11cb to i64
  %v9_11d0269 = icmp ult i32 %v3_11cb, 2
  br i1 %v9_11d0269, label %dec_label_pc_11f1, label %dec_label_pc_11d2

dec_label_pc_11c1:                                ; preds = %dec_label_pc_11d2
  %v8_11d0 = sext i32 %v4_11e5 to i64
  %v9_11d0 = icmp sgt i64 %v8_11d0, %v10_11cb
  br i1 %v9_11d0, label %dec_label_pc_11f1, label %dec_label_pc_11d2

dec_label_pc_11d2:                                ; preds = %dec_label_pc_1179, %dec_label_pc_11c1
  %storemerge4270 = phi i32 [ %v4_11e5, %dec_label_pc_11c1 ], [ 2, %dec_label_pc_1179 ]
  %v8_11d6 = zext i32 %storemerge4270 to i64
  %v10_11d6 = srem i64 %v7_11d6, %v8_11d6
  %v4_11db = trunc i64 %v10_11d6 to i32
  %v5_11db = icmp eq i32 %v4_11db, 0
  %v1_11dd = icmp eq i1 %v5_11db, false
  %v4_11e5 = add i32 %storemerge4270, 1
  br i1 %v1_11dd, label %dec_label_pc_11c1, label %dec_label_pc_1206

dec_label_pc_11f1:                                ; preds = %dec_label_pc_11c1, %dec_label_pc_1179
  store i64 %v10_11cb, i64* %rax.global-to-local, align 8
  store i64 %v10_11c6, i64* %rdx.global-to-local, align 8
  %v6_11ff = call i64 @function_1050(i64* nonnull @global_var_4060.3, i8* getelementptr inbounds ([23 x i8], [23 x i8]* @global_var_2020.5, i64 0, i64 0), i64 %v10_11c6)
  br label %dec_label_pc_1219

dec_label_pc_1206:                                ; preds = %dec_label_pc_11d2
  store i64 %v10_11d6, i64* %rax.global-to-local, align 8
  store i64 %v10_11d6, i64* %rdx.global-to-local, align 8
  %v6_1214 = call i64 @function_1050(i64* nonnull @global_var_4060.3, i8* getelementptr inbounds ([27 x i8], [27 x i8]* @global_var_2037.6, i64 0, i64 0), i64 %v10_11d6)
  br label %dec_label_pc_1219

dec_label_pc_1219:                                ; preds = %dec_label_pc_1206, %dec_label_pc_11f1
  store i64 0, i64* %rax.global-to-local, align 8
  %v1_1222 = call i64 @__readfsqword(i64 40)
  %v3_1222 = icmp eq i64 %v0_1181, %v1_1222
  br i1 %v3_1222, label %dec_label_pc_1219.dec_label_pc_1232_crit_edge, label %dec_label_pc_122d

dec_label_pc_1219.dec_label_pc_1232_crit_edge:    ; preds = %dec_label_pc_1219
  %v0_1233.pre = load i64, i64* %rax.global-to-local, align 8
  br label %dec_label_pc_1232

dec_label_pc_122d:                                ; preds = %dec_label_pc_1219
  call void @__stack_chk_fail()
  store i64 ptrtoint (i32* @0 to i64), i64* %rax.global-to-local, align 8
  br label %dec_label_pc_1232

dec_label_pc_1232:                                ; preds = %dec_label_pc_1219.dec_label_pc_1232_crit_edge, %dec_label_pc_122d
  %v0_1233 = phi i64 [ %v0_1233.pre, %dec_label_pc_1219.dec_label_pc_1232_crit_edge ], [ ptrtoint (i32* @0 to i64), %dec_label_pc_122d ]
  ret i64 %v0_1233
}

define i64 @_Z41__static_initialization_and_destruction_0ii(i64 %arg1, i64 %arg2) local_unnamed_addr {
dec_label_pc_1234:
  %rdi.global-to-local = alloca i64, align 8
  %rsi.global-to-local = alloca i64, align 8
  store i64 %arg2, i64* %rsi.global-to-local, align 8
  store i64 %arg1, i64* %rdi.global-to-local, align 8
  %v0_123c = load i64, i64* %rdi.global-to-local, align 8
  %v4_1242 = trunc i64 %v0_123c to i32
  %v14_1242 = icmp eq i32 %v4_1242, 1
  %v1_1246 = icmp eq i1 %v14_1242, false
  br i1 %v1_1246, label %dec_label_pc_127a, label %dec_label_pc_1248

dec_label_pc_1248:                                ; preds = %dec_label_pc_1234
  %v0_123f = load i64, i64* %rsi.global-to-local, align 8
  %v4_1248 = trunc i64 %v0_123f to i32
  %v14_1248 = icmp eq i32 %v4_1248, 65535
  %v1_124f = icmp eq i1 %v14_1248, false
  br i1 %v1_124f, label %dec_label_pc_127a, label %dec_label_pc_1251

dec_label_pc_1251:                                ; preds = %dec_label_pc_1248
  store i64 17049, i64* %rdi.global-to-local, align 8
  %v1_1258 = call i64 @function_1070(i64 17049)
  store i64 ptrtoint (i64* @global_var_4299 to i64), i64* %rsi.global-to-local, align 8
  %v0_126b = load void (i64*)*, void (i64*)** @global_var_3ff8.7, align 8
  %v1_126b = ptrtoint void (i64*)* %v0_126b to i64
  store i64 %v1_126b, i64* %rdi.global-to-local, align 8
  %v7_1275 = call i32 @__cxa_atexit(void (i64*)* %v0_126b, i64* nonnull @global_var_4299, i64* nonnull @global_var_4048)
  %v9_1275 = sext i32 %v7_1275 to i64
  store i64 %v9_1275, i64* @rax, align 8
  br label %dec_label_pc_127a

dec_label_pc_127a:                                ; preds = %dec_label_pc_1251, %dec_label_pc_1248, %dec_label_pc_1234
  %v0_127c = load i64, i64* @rax, align 8
  ret i64 %v0_127c
}

define i64 @_GLOBAL__sub_I_main() local_unnamed_addr {
dec_label_pc_127d:
  %v2_128b = call i64 @_Z41__static_initialization_and_destruction_0ii(i64 1, i64 65535)
  ret i64 %v2_128b
}

define i64 @__libc_csu_init() local_unnamed_addr {
dec_label_pc_12a0:
  %rax.global-to-local = alloca i64, align 8
  %tmp234 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %tmp234
}

define i64 @function_12a3(i64 %arg1, i64 %arg2, i64 %arg3) local_unnamed_addr {
dec_label_pc_12a3:
  %r12.global-to-local = alloca i64, align 8
  %r13.global-to-local = alloca i64, align 8
  %r14.global-to-local = alloca i64, align 8
  %r15.global-to-local = alloca i64, align 8
  %rbp.global-to-local = alloca i64, align 8
  %rbx.global-to-local = alloca i64, align 8
  %rdi.global-to-local = alloca i64, align 8
  %rdx.global-to-local = alloca i64, align 8
  %rsi.global-to-local = alloca i64, align 8
  store i64 %arg3, i64* %rdx.global-to-local, align 8
  store i64 %arg2, i64* %rsi.global-to-local, align 8
  store i64 %arg1, i64* %rdi.global-to-local, align 8
  %v0_12a4 = load i64, i64* %r15.global-to-local, align 8
  store i64 ptrtoint (i64* @global_var_3da8.8 to i64), i64* %r15.global-to-local, align 8
  %v0_12ad = load i64, i64* %r14.global-to-local, align 8
  %v0_12af = load i64, i64* %rdx.global-to-local, align 8
  store i64 %v0_12af, i64* %r14.global-to-local, align 8
  %v0_12b2 = load i64, i64* %r13.global-to-local, align 8
  %v0_12b4 = load i64, i64* %rsi.global-to-local, align 8
  store i64 %v0_12b4, i64* %r13.global-to-local, align 8
  %v0_12b7 = load i64, i64* %r12.global-to-local, align 8
  %v0_12b9 = load i64, i64* %rdi.global-to-local, align 8
  store i64 %v0_12b9, i64* %r12.global-to-local, align 8
  %v0_12bc = load i64, i64* %rbp.global-to-local, align 8
  %v0_12c4 = load i64, i64* %rbx.global-to-local, align 8
  store i64 sub (i64 ptrtoint (i64* @global_var_3db8.9 to i64), i64 ptrtoint (i64* @global_var_3da8.8 to i64)), i64* %rbp.global-to-local, align 8
  %v0_12cc = call i64 @_init()
  store i64 sdiv (i64 sub (i64 ptrtoint (i64* @global_var_3db8.9 to i64), i64 ptrtoint (i64* @global_var_3da8.8 to i64)), i64 8), i64* %rbp.global-to-local, align 8
  br i1 icmp eq (i64 sdiv (i64 sub (i64 ptrtoint (i64* @global_var_3db8.9 to i64), i64 ptrtoint (i64* @global_var_3da8.8 to i64)), i64 8), i64 0), label %dec_label_pc_12f6, label %dec_label_pc_12d7

dec_label_pc_12d7:                                ; preds = %dec_label_pc_12a3
  store i64 0, i64* %rbx.global-to-local, align 8
  %v0_12e0 = load i64, i64* %r14.global-to-local, align 8
  %v0_12e3 = load i64, i64* %r13.global-to-local, align 8
  %v0_12e6 = load i64, i64* %r12.global-to-local, align 8
  br label %dec_label_pc_12e0

dec_label_pc_12e0:                                ; preds = %dec_label_pc_12e0, %dec_label_pc_12d7
  %v1_12ed2 = phi i64 [ %v1_12ed, %dec_label_pc_12e0 ], [ 0, %dec_label_pc_12d7 ]
  %v1_12ed = add i64 %v1_12ed2, 1
  %v12_12f1 = icmp eq i64 %v1_12ed, sdiv (i64 sub (i64 ptrtoint (i64* @global_var_3db8.9 to i64), i64 ptrtoint (i64* @global_var_3da8.8 to i64)), i64 8)
  %v1_12f4 = icmp eq i1 %v12_12f1, false
  br i1 %v1_12f4, label %dec_label_pc_12e0, label %dec_label_pc_12f6.loopexit

dec_label_pc_12f6.loopexit:                       ; preds = %dec_label_pc_12e0
  store i64 %v0_12e0, i64* %rdx.global-to-local, align 8
  store i64 %v0_12e3, i64* %rsi.global-to-local, align 8
  store i64 %v0_12e6, i64* %rdi.global-to-local, align 8
  store i64 %v1_12ed, i64* %rbx.global-to-local, align 8
  br label %dec_label_pc_12f6

dec_label_pc_12f6:                                ; preds = %dec_label_pc_12f6.loopexit, %dec_label_pc_12a3
  store i64 %v0_12c4, i64* %rbx.global-to-local, align 8
  store i64 %v0_12bc, i64* %rbp.global-to-local, align 8
  store i64 %v0_12b7, i64* %r12.global-to-local, align 8
  store i64 %v0_12b2, i64* %r13.global-to-local, align 8
  store i64 %v0_12ad, i64* %r14.global-to-local, align 8
  store i64 %v0_12a4, i64* %r15.global-to-local, align 8
  ret i64 %v0_12cc
}

define i64 @__libc_csu_fini() local_unnamed_addr {
dec_label_pc_1310:
  %rax.global-to-local = alloca i64, align 8
  %tmp234 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %tmp234
}

define i64 @function_1313() local_unnamed_addr {
dec_label_pc_1313:
  %rax.global-to-local = alloca i64, align 8
  %v0_1314 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %v0_1314
}

define i64 @_fini() local_unnamed_addr {
dec_label_pc_1318:
  %rax.global-to-local = alloca i64, align 8
  %tmp234 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %tmp234
}

define i64 @function_131b() local_unnamed_addr {
dec_label_pc_131b:
  %rax.global-to-local = alloca i64, align 8
  %v0_1324 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %v0_1324
}

declare void @__cxa_finalize(i64*) local_unnamed_addr

declare void @__gmon_start__() local_unnamed_addr

declare i64 @_ZNSirsERi() local_unnamed_addr

declare i32 @__cxa_atexit(void (i64*)*, i64*, i64*) local_unnamed_addr

declare i64 @_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc() local_unnamed_addr

declare void @__stack_chk_fail() local_unnamed_addr

declare i64 @_ZNSt8ios_base4InitC1Ev() local_unnamed_addr

declare i64 @__readfsqword(i64) local_unnamed_addr
