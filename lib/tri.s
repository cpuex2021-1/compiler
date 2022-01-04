fsgnj: # val of f0, sign of f1
    flt a0, fzero, f0
    flt a1, fzero, f1
    beq a0, a1, fsgnj_l1
    fneg f0, f0
fsgnj_l1:
    jalr zero, ra, 0
kernel_sin: #f0=x f1=flag
    fmul f2, f0, f0
    fmul f3, f0, f2
    fmul f5, f2, f3
    fmul f4, f2, f4
    fli f6, 0.16666668
    fmul f6, f3, f6
    fsub f6, f0, f6
    fli f7, 0.008332824
    fmul f7, f4, f7
    fli f8, 0.00019587841
    fmul f8, f5, f8
    fsub f8, f7, f8
    fadd f0, f6, f8
    jump fsgnj
kernel_cos: #f0=x f1=flag
    fmul f2, f0, f0
    fmul f3, f2, f2
    fmul f4, f2, f3
    fli f5, 0.5
    fmul f5, f1, f5
    fli f6, 1.0
    fsub f6, f6, f5
    fli f7, 0.04166368
    fmul f7, f3, f7
    fli f8, 0.0013695068
    fmul f8, f4, f8
    fsub f8, f7, f8
    fadd f0, f6, f8
    jump fsgnj
reduction_2pi: #f0=x f1=p a0=is_p_updating
	beq a0, zero, reduction_2pi_l1
	flt a1, f0, f1
    bne a1, zero, reduction_2pi_l0_1
    fli f2, 2.0
    fmul f1, f1, f2
    jump reduction_2pi
reduction_2pi_l0_1:
    fli f2, 6.2831853
    flt a1, f0, f2
    bne a1, zero, reduction_2pi_l0_3
    flt a1, f0, f1
    bne a1, zero, reduction_2pi_l0_2
    fsub f0, f0, f1
    fli f2, 2.0
    fdiv f1, f1, f2
    li a0, 0
    jump reduction_2pi
reduction_2pi_l0_2:
    fli f2, 2.0
    fdiv f1, f1, f2
    li a0, 0
    jump reduction_2pi
reduction_2pi_l0_3:
    jalr zero, ra, 0
reduction_2pi_l1:
	fli f2, 6.2831853
    flt a1, f0, f2
    bne a1, zero, reduction_2pi_l1_2
    flt a1, f0, f1
    bne a1, zero, reduction_2pi_l1_1
    fsub f0, f0, f1
    fli f2, 2.0
    fdiv f1, f1, f2
    li a0, 0
    jump reduction_2pi
reduction_2pi_l1_1:
    fli f2, 2.0
    fdiv f1, f1, f2
    li a0, 0
    jump reduction_2pi
reduction_2pi_l1_2:
    jalr zero, ra, 0
sin:
    fsw f0, 0(sp)
	sw ra, -1(sp)
	addi sp, sp, -2
	jal ra, fabs # call
	addi sp, sp, 2
    lw ra, -1(sp)
	fli f1, 6.2831853
	li a0, 1
	addi sp, sp, -2
	jal ra, reduction_2pi # call
	addi sp, sp, 2
	lw ra, -1(sp)
	flw f1, 0(sp)
    # f0=x, f1=x_orig
    fli f2, 3.14159265
    flt a1, f0, f2
    bne a1, zero, sin_l1
    fli f3, 1.570796325
    flt a1, f0, f3
    bne a1, zero, sin_l0_1
    fli f4, 5.49778713750000048
    flt a1, f0, f4
    bne a1, zero, sin_l0_0_1
    fli f2, 6.2831853
    fsub f0, f2, f0
    fneg f1, f1
    jump kernel_sin
sin_l0_0_1:
    fli f3, 4.71238897500000053
    fsub f0, f0, f3
    fneg f1, f1
    jump kernel_cos
sin_l0_1:
    fli f4, 3.9269908125
    flt a1, f0, f4
    bne a1, zero, sin_l0_2
    fsub f0, f0, f2
    fneg f1, f1
    jump kernel_sin
sin_l0_2:
    fli f4, 4.71238897500000053
    fsub f0, f4, f0
    fneg f1, f1
    jump kernel_cos
sin_l1:
    fli f3, 1.570796325
    flt a1, f0, f3
    bne a1, zero, sin_l2
    fli f4, 2.35619448750000027
    flt a1, f0, f4
    bne a1, zero, sin_l1_1
    fsub f0, f2, f0
    jump kernel_sin
sin_l1_1:
    fsub f0, f0, f3
    jump kernel_cos
sin_l2:
    fli f3, 0.7853981625
    flt a1, f0, f3
    bne a1, zero, sin_l3
    jump kernel_sin
sin_l3:
    fsub f0, f2, f0
    jump kernel_cos
cos:
    fsw f0, 0(sp)
	sw ra, -1(sp)
	addi sp, sp, -2
	jal ra, fabs # call
	addi sp, sp, 2
    lw ra, -1(sp)
	fli f1, 6.2831853
	li a0, 1
	addi sp, sp, -2
	jal ra, reduction_2pi # call
	addi sp, sp, 2
	lw ra, -1(sp)
	flw f1, 0(sp)
    # f0=x, f1=x_orig
    fli f2, 1.570796325
    flt a1, f0, f2
    bne a1, zero, cos_l1
    fli f3, 4.71238897500000053
    flt a1, f0, f3
    bne a1, zero, cos_l0_1
    fli f4, 5.49778713750000048
    flt a1, f0, f4
    bne a1, zero, cos_l0_0_1
    fli f2, 6.2831853
    fsub f0, f2, f0
    fli f1, -1.0
    jump kernel_cos
cos_l0_0_1:
    fli f3, 5.49778713750000048
    fsub f0, f0, f3
    fli f1, -1.0
    jump kernel_sin
cos_l0_1:
    fli f4, 2.51327412
    flt a1, f4, f0
    bne a1, zero, cos_l0_2
    fli f2, 3.14159265
    fsub f0, f0, f2
    fli f1, 1.0
    jump kernel_cos
cos_l0_2:
    fli f4, 4.71238897500000053
    fsub f0, f4, f0
    fli f1, 1.0
    jump kernel_sin
cos_l1:
    fli f3, 1.570796325
    flt a1, f0, f3
    bne a1, zero, cos_l2
    fli f4, 2.35619448750000027
    flt a1, f0, f4
    bne a1, zero, cos_l1_1
    fli f2, 3.14159265
    fsub f0, f2, f0
    fli f1, -1.0
    jump kernel_cos
cos_l1_1:
    fsub f0, f0, f3
    fli f1, -1.0
    jump kernel_sin
cos_l2:
    fli f3, 0.7853981625
    flt a1, f3, f0
    bne a1, zero, cos_l3
    fli f1, 1.0
    jump kernel_cos
cos_l3:
    fsub f0, f2, f0
    fli f1, 1.0
    jump kernel_sin
kernel_atan: # f0=x f1=flag
	fmul f2, f0, f0
	fmul f3, f0, f2
	fmul f4, f3, f2
	fmul f5, f4, f2
	fmul f6, f5, f2
	fmul f7, f6, f2
	fmul f2, f7, f2
	fli f8, 0.3333333
	fmul f3, f8, f3
	fsub f0, f0, f3
	fli f3, 0.2
	fmul f3, f3, f4
	fli f4, 0.142857142
	fmul f4, f4, f5
	fsub f3, f3, f4
	fli f4, 0.111111104
	fmul f4, f4, f6
	fli f5, 0.08976446
	fmul f5, f5, f7
	fsub f4, f4, f5
	fli f5, 0.060035485
	fmul f2, f5, f2
	fadd f2, f4, f2
	fadd f0, f0, f3
	fadd f0, f0, f2
	jump fsgnj
atan:
	fsw f0, 0(sp)
	sw ra, -1(sp)
	addi sp, sp, -2
	jal ra, fabs # call
	addi sp, sp, 2
	lw ra, -1(sp)
    flw f1, 0(sp)
    # f0=a f1=x
	fli f2, 0.4375
    fle a1, f2, f0
    bne a1, zero, atan_l1
    fmv f0, f1
    jump kernel_atan
atan_l1:
    fli f2, 2.4375
    fle a1, f2, f0
    bne a1, zero, atan_l2
    fli f3, 1.0
    fsub f4, f0, f3
    fadd f5, f0, f3
    fdiv f0, f4, f5
    jump kernel_atan
atan_l2:
    fli f2, 1.0
    fdiv f0, f2, f0
    jump kernel_atan