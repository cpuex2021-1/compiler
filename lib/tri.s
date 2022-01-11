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
	fmul f4, f3, f2
	fmul f2, f4, f2
	fli f5, 0.166667
	fmul f3, f5, f3
	fsub f0, f0, f3
	fli f3, 0.008333
	fmul f3, f3, f4
	fli f4, 0.000196
	fmul f2, f4, f2
	fsub f2, f3, f2
	fadd f0, f0, f2
	jump fsgnj
kernel_cos: #f0=x f1=flag
	fmul f0, f0, f0
	fmul f2, f0, f0
	fmul f3, f2, f0
	fli f4, 1.000000
	fli f5, 0.500000
	fmul f0, f5, f0
	fsub f0, f4, f0
	fli f4, 0.041664
	fmul f2, f4, f2
	fli f4, 0.001370
	fmul f3, f4, f3
	fsub f2, f2, f3
	fadd f0, f0, f2
	jump fsgnj
reduction_2pi: #f0=x f1=p a0=is_p_updating
	li a1, 0
	bne a0, a1, be_else.460
	fli f2, 3.141593
	fli f3, 2.000000
	fmul f2, f2, f3
    flt a2, f0, f2
	bne a2, zero, fble_else.461
    flt a2, f0, f1
	bne a2, zero, fble_else.462
	fsub f0, f0, f1
	fli f2, 2.000000
	fdiv f1, f1, f2
	li a0, 0
	jump reduction_2pi
fble_else.462:
	fli f2, 2.000000
	fdiv f1, f1, f2
	li a0, 0
	jump reduction_2pi
fble_else.461:
	jalr zero, ra, 0 # ret
be_else.460:
    flt a2, f0, f1
	bne a2, zero, fble_else.463
	fli f2, 2.000000
	fmul f1, f1, f2
	li a0, 1
	jump reduction_2pi
fble_else.463:
	fli f2, 3.141593
	fli f3, 2.000000
	fmul f2, f2, f3
    flt a2, f0, f2
	bne a2, zero, fble_else.464
	flt a2, f0, f1
    bne a2, zero, fble_else.465
	fsub f0, f0, f1
	fli f2, 2.000000
	fdiv f1, f1, f2
	li a0, 0
	jump reduction_2pi
fble_else.465:
	fli f2, 2.000000
	fdiv f1, f1, f2
	li a0, 0
	jump reduction_2pi
fble_else.464:
	jalr zero, ra, 0 # ret
sin:
	fsw f0, 0(sp)
	sw ra, -2(sp)
	addi sp, sp, -3
	jal ra, fabs # call
	addi sp, sp, 3
	lw ra, -2(sp)
	fli f1, 2.000000
	fli f2, 3.141593
	fmul f1, f1, f2
	li a0, 1
	sw ra, -2(sp)
	addi sp, sp, -3
	jal ra, reduction_2pi # call
	addi sp, sp, 3
	lw ra, -2(sp)
	fli f1, 3.141593
    flt a2, f0, f1
	bne a2, zero, fble_else.466
	fli f1, 3.141593
	fli f2, 2.000000
	fdiv f1, f1, f2
    flt a2, f0, f1
	bne a2, zero, fble_else.467
	fli f1, 3.141593
	fli f2, 4.000000
	fdiv f1, f1, f2
	fli f2, 7.000000
	fmul f1, f1, f2
    flt a2, f0, f1
	bne a2, zero, fble_else.468
	fli f1, 3.141593
	fli f2, 2.000000
	fmul f1, f1, f2
	fsub f0, f1, f0
	lw a0, 0(sp)
	sub f1, zero, a0
	jump kernel_sin
fble_else.468:
	fli f1, 3.141593
	fli f2, 3.000000
	fmul f1, f1, f2
	fli f2, 2.000000
	fdiv f1, f1, f2
	fsub f0, f0, f1
	lw a0, 0(sp)
	sub f1, zero, a0
	jump kernel_cos
fble_else.467:
	fli f1, 3.141593
	fli f2, 4.000000
	fdiv f1, f1, f2
	fli f2, 5.000000
	fmul f1, f1, f2
    flt a2, f0, f1
	bne a2, zero, fble_else.469
	fli f1, 3.141593
	fsub f0, f0, f1
	lw a0, 0(sp)
	sub f1, zero, a0
	jump kernel_sin
fble_else.469:
	fli f1, 3.141593
	fli f2, 3.000000
	fmul f1, f1, f2
	fli f2, 2.000000
	fdiv f1, f1, f2
	fsub f0, f1, f0
	lw a0, 0(sp)
	sub f1, zero, a0
	jump kernel_cos
fble_else.466:
	fli f1, 3.141593
	fli f2, 2.000000
	fdiv f1, f1, f2
    flt a2, f0, f1
	bne a2, zero, fble_else.470
	fli f1, 3.141593
	fli f2, 4.000000
	fdiv f1, f1, f2
	fli f2, 3.000000
	fmul f1, f1, f2
    flt a2, f0, f1
	bne a2, zero, fble_else.471
	fli f1, 3.141593
	fsub f0, f1, f0
	flw f1, 0(sp)
	jump kernel_sin
fble_else.471:
	fli f1, 3.141593
	fli f2, 2.000000
	fdiv f1, f1, f2
	fsub f0, f0, f1
	flw f1, 0(sp)
	jump kernel_cos
fble_else.470:
	fli f1, 3.141593
	fli f2, 4.000000
	fdiv f1, f1, f2
    flt a2, f0, f1
	bne a2, zero, fble_else.472
	flw f1, 0(sp)
	jump kernel_sin
fble_else.472:
	fli f1, 3.141593
	fli f2, 2.000000
	fdiv f1, f1, f2
	fsub f0, f1, f0
	flw f1, 0(sp)
	jump kernel_cos
cos:
	sw ra, 0(sp)
	addi sp, sp, -1
	jal ra, fabs # call
	addi sp, sp, 1
	lw ra, 0(sp)
	fli f1, 2.000000
	fli f2, 3.141593
	fmul f1, f1, f2
	li a0, 1
	sw ra, 0(sp)
	addi sp, sp, -1
	jal ra, reduction_2pi # call
	addi sp, sp, 1
	lw ra, 0(sp)
	fli f1, 3.141593
	fli f2, 2.000000
	fdiv f1, f1, f2
    flt a2, f0, f1
	bne a2, zero, fble_else.473
	fli f1, 3.141593
	fli f2, 2.000000
	fdiv f1, f1, f2
	fli f2, 3.000000
	fmul f1, f1, f2
    flt a2, f0, f1
	bne a2, zero, fble_else.474
	fli f1, 3.141593
	fli f2, 7.000000
	fmul f1, f1, f2
	fli f2, 4.000000
	fdiv f1, f1, f2
    flt a2, f0, f1
	bne a2, zero, fble_else.475
	fli f1, 2.000000
	fli f2, 3.141593
	fmul f1, f1, f2
	fsub f0, f1, f0
	fli f1, -1.000000
	jump kernel_cos
fble_else.475:
	fli f1, 3.141593
	fli f2, 7.000000
	fmul f1, f1, f2
	fli f2, 4.000000
	fdiv f1, f1, f2
	fsub f0, f0, f1
	fli f1, -1.000000
	jump kernel_sin
fble_else.474:
	fli f1, 3.141593
	fli f2, 5.000000
	fdiv f1, f1, f2
	fli f2, 4.000000
	fmul f1, f1, f2
    flt a2, f1, f0
	bne a2, zero, fble_else.476
	fli f1, 3.141593
	fsub f0, f0, f1
	fli f1, 1.000000
	jump kernel_cos
fble_else.476:
	fli f1, 3.141593
	fli f2, 3.000000
	fmul f1, f1, f2
	fli f2, 2.000000
	fdiv f1, f1, f2
	fsub f0, f1, f0
	fli f1, 1.000000
	jump kernel_sin
fble_else.473:
	fli f1, 3.141593
	fli f2, 2.000000
	fdiv f1, f1, f2
    flt a2, f0, f1
	bne a2, zero, fble_else.477
	fli f1, 3.141593
	fli f2, 3.000000
	fmul f1, f1, f2
	fli f2, 4.000000
	fdiv f1, f1, f2
    flt a2, f0, f1
	bne a2, zero, fble_else.478
	fli f1, 3.141593
	fsub f0, f1, f0
	fli f1, -1.000000
	jump kernel_cos
fble_else.478:
	fli f1, 3.141593
	fli f2, 2.000000
	fdiv f1, f1, f2
	fsub f0, f0, f1
	fli f1, -1.000000
	jump kernel_sin
fble_else.477:
	fli f1, 3.141593
	fli f2, 4.000000
	fdiv f1, f1, f2
    flt a2, f1, f0
	bne a2, zero, fble_else.479
	fli f1, 1.000000
	jump kernel_cos
fble_else.479:
	fli f1, 3.141593
	fli f2, 2.000000
	fdiv f1, f1, f2
	fsub f0, f1, f0
	fli f1, 1.000000
	jump kernel_sin
kernel_atan: # f0=x f1=flag
	fmul f1, f0, f0
	fmul f3, f0, f1
	fmul f4, f3, f1
	fmul f5, f4, f1
	fmul f6, f5, f1
	fmul f7, f6, f1
	fmul f1, f7, f1
	fli f8, 0.333333
	fmul f3, f8, f3
	fsub f0, f0, f3
	fli f3, 0.200000
	fmul f3, f3, f4
	fli f4, 0.142857
	fmul f4, f4, f5
	fsub f3, f3, f4
	fli f4, 0.111111
	fmul f4, f4, f6
	fli f5, 0.089764
	fmul f5, f5, f7
	fsub f4, f4, f5
	fli f5, 0.060035
	fmul f1, f5, f1
	fadd f1, f4, f1
	fadd f0, f0, f3
	fadd f0, f0, f1
	fmv f1, f2
	jump fsgnj
atan:
	fsw f0, 0(sp)
	sw ra, -2(sp)
	addi sp, sp, -3
	jal ra, fabs # call
	addi sp, sp, 3
	lw ra, -2(sp)
	fli f1, 0.437500
    flt a2, f0, f1
	bne a2, zero, fble_else.480
	fli f1, 2.437500
    flt a2, f0, f1
	bne a2, zero, fble_else.481
	fli f1, 1.000000
	fdiv f0, f1, f0
	fli f1, -3.141593
	fli f2, 2.000000
	fdiv f1, f1, f2
	flw f2, 0(sp)
	jump kernel_atan
fble_else.481:
	fli f1, 1.000000
	fsub f1, f0, f1
	fli f2, 1.000000
	fadd f0, f0, f2
	fdiv f0, f1, f0
	fli f1, 3.141593
	fli f2, 4.000000
	fdiv f1, f1, f2
	flw f2, 0(sp)
	jump kernel_atan
fble_else.480:
	fli f1, 0.000000
	flw f0, 0(sp)
	fmv f2, f0
	jump kernel_atan