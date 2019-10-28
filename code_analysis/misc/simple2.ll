source_filename = "test"
target datalayout = "e-m:e-p:64:64-i64:64-f80:128-n8:16:32:64-S128"

@rax = internal unnamed_addr global i64 0
@global_var_4028.1 = global i64 0
@global_var_3df8.3 = global i64 4368
@global_var_3e00.4 = global i64 4288
@0 = external global i32

define i64 @_init() local_unnamed_addr {
dec_label_pc_1000:
  %tmp = load i64, i64* @rax, align 8
  ret i64 %tmp
}

define i64 @function_1003() local_unnamed_addr {
dec_label_pc_1003:
  %v0_1008 = load i64, i64* inttoptr (i64 16368 to i64*), align 16
  %v1_100f = icmp eq i64 %v0_1008, 0
  br i1 %v1_100f, label %dec_label_pc_1016, label %dec_label_pc_1014

dec_label_pc_1014:                                ; preds = %dec_label_pc_1003
  call void @__gmon_start__()
  br label %dec_label_pc_1016

dec_label_pc_1016:                                ; preds = %dec_label_pc_1014, %dec_label_pc_1003
  %v0_101a = phi i64 [ ptrtoint (i32* @0 to i64), %dec_label_pc_1014 ], [ 0, %dec_label_pc_1003 ]
  ret i64 %v0_101a
}

define i64 @_start() local_unnamed_addr {
dec_label_pc_1020:
  %rax.global-to-local = alloca i64, align 8
  %tmp234 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %tmp234
}

define i64 @deregister_tm_clones() local_unnamed_addr {
dec_label_pc_1050:
  ret i64 ptrtoint (i64* @global_var_4028.1 to i64)
}

define i64 @register_tm_clones() local_unnamed_addr {
dec_label_pc_1080:
  ret i64 0
}

define i64 @__do_global_dtors_aux() local_unnamed_addr {
dec_label_pc_10c0:
  %rax.global-to-local = alloca i64, align 8
  %tmp234 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %tmp234
}

define i64 @function_10c3() local_unnamed_addr {
dec_label_pc_10c3:
  %rax.global-to-local = alloca i64, align 8
  %rbp.global-to-local = alloca i64, align 8
  %stack_var_-8 = alloca i64, align 8
  %v0_10c4 = load i8, i8* bitcast (i64* @global_var_4028.1 to i8*), align 8
  %v7_10c4 = icmp eq i8 %v0_10c4, 0
  %v1_10cb = icmp eq i1 %v7_10c4, false
  br i1 %v1_10cb, label %dec_label_pc_1100, label %dec_label_pc_10cd

dec_label_pc_10cd:                                ; preds = %dec_label_pc_10c3
  %v0_10cd = load i64, i64* %rbp.global-to-local, align 8
  store i64 %v0_10cd, i64* %stack_var_-8, align 8
  %v4_10cd = ptrtoint i64* %stack_var_-8 to i64
  %v0_10ce = load i64, i64* inttoptr (i64 16344 to i64*), align 8
  %v7_10ce = icmp eq i64 %v0_10ce, 0
  store i64 %v4_10cd, i64* %rbp.global-to-local, align 8
  br i1 %v7_10ce, label %dec_label_pc_10e8, label %dec_label_pc_10db

dec_label_pc_10db:                                ; preds = %dec_label_pc_10cd
  %v0_10db = load i64, i64* inttoptr (i64 16416 to i64*), align 32
  %v1_10e2 = inttoptr i64 %v0_10db to i64*
  call void @__cxa_finalize(i64* %v1_10e2)
  store i64 ptrtoint (i32* @0 to i64), i64* %rax.global-to-local, align 8
  br label %dec_label_pc_10e8

dec_label_pc_10e8:                                ; preds = %dec_label_pc_10db, %dec_label_pc_10cd
  %v0_10e8 = call i64 @deregister_tm_clones()
  store i64 %v0_10e8, i64* %rax.global-to-local, align 8
  store i8 1, i8* bitcast (i64* @global_var_4028.1 to i8*), align 8
  %v2_10f4 = load i64, i64* %stack_var_-8, align 8
  store i64 %v2_10f4, i64* %rbp.global-to-local, align 8
  ret i64 %v0_10e8

dec_label_pc_1100:                                ; preds = %dec_label_pc_10c3
  %v0_1100 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %v0_1100
}

define i64 @frame_dummy() local_unnamed_addr {
dec_label_pc_1110:
  %rax.global-to-local = alloca i64, align 8
  %tmp234 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %tmp234
}

define i64 @function_1113() local_unnamed_addr {
dec_label_pc_1113:
  %v0_1114 = call i64 @register_tm_clones()
  ret i64 %v0_1114
}

define i64 @_Z3funi(i64 %arg1) local_unnamed_addr {
dec_label_pc_1119:
  %v3_1126 = mul i64 %arg1, 818089009
  %v2_112d = udiv i64 %v3_1126, 4294967296
  %v1_1131 = trunc i64 %v2_112d to i32
  %v3_1131 = sdiv i32 %v1_1131, 8
  %v1_1134 = trunc i64 %arg1 to i32
  %v3_1134 = ashr i32 %v1_1134, 31
  %v4_1137 = sub nsw i32 %v3_1131, %v3_1134
  %v20_1137 = zext i32 %v4_1137 to i64
  ret i64 %v20_1137
}

define i64 @main(i64 %argc, i8** %argv) local_unnamed_addr {
dec_label_pc_1143:
  %v1_114c = call i64 @_Z3funi(i64 7)
  ret i64 0
}

define i64 @__libc_csu_init() local_unnamed_addr {
dec_label_pc_1160:
  %rax.global-to-local = alloca i64, align 8
  %tmp234 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %tmp234
}

define i64 @function_1163(i64 %arg1, i64 %arg2, i64 %arg3) local_unnamed_addr {
dec_label_pc_1163:
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
  %v0_1164 = load i64, i64* %r15.global-to-local, align 8
  store i64 ptrtoint (i64* @global_var_3df8.3 to i64), i64* %r15.global-to-local, align 8
  %v0_116d = load i64, i64* %r14.global-to-local, align 8
  %v0_116f = load i64, i64* %rdx.global-to-local, align 8
  store i64 %v0_116f, i64* %r14.global-to-local, align 8
  %v0_1172 = load i64, i64* %r13.global-to-local, align 8
  %v0_1174 = load i64, i64* %rsi.global-to-local, align 8
  store i64 %v0_1174, i64* %r13.global-to-local, align 8
  %v0_1177 = load i64, i64* %r12.global-to-local, align 8
  %v0_1179 = load i64, i64* %rdi.global-to-local, align 8
  store i64 %v0_1179, i64* %r12.global-to-local, align 8
  %v0_117c = load i64, i64* %rbp.global-to-local, align 8
  %v0_1184 = load i64, i64* %rbx.global-to-local, align 8
  store i64 sub (i64 ptrtoint (i64* @global_var_3e00.4 to i64), i64 ptrtoint (i64* @global_var_3df8.3 to i64)), i64* %rbp.global-to-local, align 8
  %v0_118c = call i64 @_init()
  store i64 sdiv (i64 sub (i64 ptrtoint (i64* @global_var_3e00.4 to i64), i64 ptrtoint (i64* @global_var_3df8.3 to i64)), i64 8), i64* %rbp.global-to-local, align 8
  br i1 icmp eq (i64 sdiv (i64 sub (i64 ptrtoint (i64* @global_var_3e00.4 to i64), i64 ptrtoint (i64* @global_var_3df8.3 to i64)), i64 8), i64 0), label %dec_label_pc_11b6, label %dec_label_pc_1197

dec_label_pc_1197:                                ; preds = %dec_label_pc_1163
  store i64 0, i64* %rbx.global-to-local, align 8
  %v0_11a0 = load i64, i64* %r14.global-to-local, align 8
  %v0_11a3 = load i64, i64* %r13.global-to-local, align 8
  %v0_11a6 = load i64, i64* %r12.global-to-local, align 8
  br label %dec_label_pc_11a0

dec_label_pc_11a0:                                ; preds = %dec_label_pc_11a0, %dec_label_pc_1197
  %v1_11ad2 = phi i64 [ %v1_11ad, %dec_label_pc_11a0 ], [ 0, %dec_label_pc_1197 ]
  %v1_11ad = add i64 %v1_11ad2, 1
  %v12_11b1 = icmp eq i64 %v1_11ad, sdiv (i64 sub (i64 ptrtoint (i64* @global_var_3e00.4 to i64), i64 ptrtoint (i64* @global_var_3df8.3 to i64)), i64 8)
  %v1_11b4 = icmp eq i1 %v12_11b1, false
  br i1 %v1_11b4, label %dec_label_pc_11a0, label %dec_label_pc_11b6.loopexit

dec_label_pc_11b6.loopexit:                       ; preds = %dec_label_pc_11a0
  store i64 %v0_11a0, i64* %rdx.global-to-local, align 8
  store i64 %v0_11a3, i64* %rsi.global-to-local, align 8
  store i64 %v0_11a6, i64* %rdi.global-to-local, align 8
  store i64 %v1_11ad, i64* %rbx.global-to-local, align 8
  br label %dec_label_pc_11b6

dec_label_pc_11b6:                                ; preds = %dec_label_pc_11b6.loopexit, %dec_label_pc_1163
  store i64 %v0_1184, i64* %rbx.global-to-local, align 8
  store i64 %v0_117c, i64* %rbp.global-to-local, align 8
  store i64 %v0_1177, i64* %r12.global-to-local, align 8
  store i64 %v0_1172, i64* %r13.global-to-local, align 8
  store i64 %v0_116d, i64* %r14.global-to-local, align 8
  store i64 %v0_1164, i64* %r15.global-to-local, align 8
  ret i64 %v0_118c
}

define i64 @__libc_csu_fini() local_unnamed_addr {
dec_label_pc_11d0:
  %rax.global-to-local = alloca i64, align 8
  %tmp234 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %tmp234
}

define i64 @function_11d3() local_unnamed_addr {
dec_label_pc_11d3:
  %rax.global-to-local = alloca i64, align 8
  %v0_11d4 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %v0_11d4
}

define i64 @_fini() local_unnamed_addr {
dec_label_pc_11d8:
  %rax.global-to-local = alloca i64, align 8
  %tmp234 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %tmp234
}

define i64 @function_11db() local_unnamed_addr {
dec_label_pc_11db:
  %rax.global-to-local = alloca i64, align 8
  %v0_11e4 = load i64, i64* %rax.global-to-local, align 8
  ret i64 %v0_11e4
}

declare void @__cxa_finalize(i64*) local_unnamed_addr

declare void @__gmon_start__() local_unnamed_addr
