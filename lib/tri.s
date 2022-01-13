pi_div.122:
	fli f2, 0.000000
	feq a20, f2, f0
	beq a20, zero, fbe_else.313
	jalr zero, ra, 0 # ret
fbe_else.313:
	fli f2, 0.000000
	fle a20, f0, f2
	beq a20, zero, fble_else.314
	fle a20, f0, f1
	beq a20, zero, fble_else.315
	fsw f1, 0(sp)
	fsw f0, -2(sp)
	fmv f0, f1
	sw ra, -4(sp)
	addi sp, sp, -5
	jal ra, fhalf # call
	addi sp, sp, 5
	lw ra, -4(sp)
	flw f1, -2(sp)
	fsub f0, f1, f0
	flw f1, 0(sp)
	fsw f0, -4(sp)
	fmv f0, f1
	sw ra, -6(sp)
	addi sp, sp, -7
	jal ra, fhalf # call
	addi sp, sp, 7
	lw ra, -6(sp)
	fadd f1, a0, fzero
	flw f0, -4(sp)
	jump pi_div.122
fble_else.315:
	fli f2, 2.000000
	fmul f1, f1, f2
	jump pi_div.122
fble_else.314:
	fli f2, 3.141593
	fli f3, 2.000000
	fmul f2, f2, f3
	fle a20, f2, f0
	beq a20, zero, fble_else.316
	fle a20, f0, f1
	beq a20, zero, fble_else.317
	fsw f1, 0(sp)
	fsw f0, -2(sp)
	fmv f0, f1
	sw ra, -6(sp)
	addi sp, sp, -7
	jal ra, fhalf # call
	addi sp, sp, 7
	lw ra, -6(sp)
	flw f1, -2(sp)
	fsub f0, f1, f0
	flw f1, 0(sp)
	fsw f0, -6(sp)
	fmv f0, f1
	sw ra, -8(sp)
	addi sp, sp, -9
	jal ra, fhalf # call
	addi sp, sp, 9
	lw ra, -8(sp)
	fadd f1, a0, fzero
	flw f0, -6(sp)
	jump pi_div.122
fble_else.317:
	fli f2, 2.000000
	fmul f1, f1, f2
	jump pi_div.122
fble_else.316:
	jalr zero, ra, 0 # ret
pi4div.125:
	fli f1, 3.141593
	fli f2, 2.000000
	fdiv f1, f1, f2
	fle a20, f1, f0
	beq a20, zero, fble_else.318
	fli f1, 3.141593
	fle a20, f1, f0
	beq a20, zero, fble_else.319
	fli f1, 3.141593
	fli f2, 1.500000
	fmul f1, f1, f2
	fle a20, f1, f0
	beq a20, zero, fble_else.320
	fli f1, 3.141593
	fli f2, 2.000000
	fmul f1, f1, f2
	fsub f0, f1, f0
	fli f1, 1.000000
	add a0, hp, zero
	addi hp, hp, 8
	fsw f1, 4(a0)
	fsw f0, 0(a0)
	jalr zero, ra, 0 # ret
fble_else.320:
	fli f1, 3.141593
	fsub f0, f0, f1
	fli f1, -1.000000
	add a0, hp, zero
	addi hp, hp, 8
	fsw f1, 4(a0)
	fsw f0, 0(a0)
	jalr zero, ra, 0 # ret
fble_else.319:
	fli f1, 3.141593
	fsub f0, f1, f0
	fli f1, -1.000000
	add a0, hp, zero
	addi hp, hp, 8
	fsw f1, 4(a0)
	fsw f0, 0(a0)
	jalr zero, ra, 0 # ret
fble_else.318:
	fli f1, 1.000000
	add a0, hp, zero
	addi hp, hp, 8
	fsw f1, 4(a0)
	fsw f0, 0(a0)
	jalr zero, ra, 0 # ret
pi4div2.127:
	fli f1, 3.141593
	fli f2, 2.000000
	fdiv f1, f1, f2
	fle a20, f1, f0
	beq a20, zero, fble_else.321
	fli f1, 3.141593
	fle a20, f1, f0
	beq a20, zero, fble_else.322
	fli f1, 3.141593
	fli f2, 1.500000
	fmul f1, f1, f2
	fle a20, f1, f0
	beq a20, zero, fble_else.323
	fli f1, 3.141593
	fli f2, 2.000000
	fmul f1, f1, f2
	fsub f0, f1, f0
	fli f1, -1.000000
	add a0, hp, zero
	addi hp, hp, 8
	fsw f1, 4(a0)
	fsw f0, 0(a0)
	jalr zero, ra, 0 # ret
fble_else.323:
	fli f1, 3.141593
	fsub f0, f0, f1
	fli f1, -1.000000
	add a0, hp, zero
	addi hp, hp, 8
	fsw f1, 4(a0)
	fsw f0, 0(a0)
	jalr zero, ra, 0 # ret
fble_else.322:
	fli f1, 3.141593
	fsub f0, f1, f0
	fli f1, 1.000000
	add a0, hp, zero
	addi hp, hp, 8
	fsw f1, 4(a0)
	fsw f0, 0(a0)
	jalr zero, ra, 0 # ret
fble_else.321:
	fli f1, 1.000000
	add a0, hp, zero
	addi hp, hp, 8
	fsw f1, 4(a0)
	fsw f0, 0(a0)
	jalr zero, ra, 0 # ret
tailor_cos.129:
	fmul f0, f0, f0
	fsw f0, 0(sp)
	sw ra, -2(sp)
	addi sp, sp, -3
	jal ra, fhalf # call
	addi sp, sp, 3
	lw ra, -2(sp)
	flw f1, 0(sp)
	fmul f2, f1, f0
	fli f3, 0.083333
	fmul f2, f2, f3
	fmul f3, f1, f2
	fli f4, 0.033333
	fmul f3, f3, f4
	fmul f4, f1, f3
	fli f5, 0.017857
	fmul f4, f4, f5
	fmul f5, f1, f4
	fli f6, 0.011111
	fmul f5, f5, f6
	fmul f1, f1, f5
	fli f6, 0.007576
	fmul f1, f1, f6
	fli f6, 1.000000
	fsub f0, f6, f0
	fadd f0, f0, f2
	fsub f0, f0, f3
	fadd f0, f0, f4
	fsub f0, f0, f5
	fadd f0, f0, f1
	jalr zero, ra, 0 # ret
cos:
	fli f1, 3.141593
	fli f2, 2.000000
	fmul f1, f1, f2
	sw ra, 0(sp)
	addi sp, sp, -1
	jal ra, pi_div.122 # call
	addi sp, sp, 1
	lw ra, 0(sp)
	sw ra, 0(sp)
	addi sp, sp, -1
	jal ra, pi4div.125 # call
	addi sp, sp, 1
	lw ra, 0(sp)
	flw f0, 4(a0)
	flw f1, 0(a0)
	fsw f0, 0(sp)
	fmv f0, f1
	sw ra, -2(sp)
	addi sp, sp, -3
	jal ra, tailor_cos.129 # call
	addi sp, sp, 3
	lw ra, -2(sp)
	flw f1, 0(sp)
	fmul f0, f1, f0
	jalr zero, ra, 0 # ret
sin:
	fli f1, 3.141593
	fli f2, 2.000000
	fmul f1, f1, f2
	sw ra, 0(sp)
	addi sp, sp, -1
	jal ra, pi_div.122 # call
	addi sp, sp, 1
	lw ra, 0(sp)
	sw ra, 0(sp)
	addi sp, sp, -1
	jal ra, pi4div2.127 # call
	addi sp, sp, 1
	lw ra, 0(sp)
	flw f0, 4(a0)
	flw f1, 0(a0)
	fli f2, 3.141593
	fli f3, 2.000000
	fdiv f2, f2, f3
	fsub f1, f2, f1
	fsw f0, 0(sp)
	fmv f0, f1
	sw ra, -2(sp)
	addi sp, sp, -3
	jal ra, tailor_cos.129 # call
	addi sp, sp, 3
	lw ra, -2(sp)
	flw f1, 0(sp)
	fmul f0, f1, f0
	jalr zero, ra, 0 # ret
tailor_atan.135:
	fmul f1, f0, f0
	fmul f2, f1, f0
	fli f3, 0.333333
	fmul f2, f2, f3
	fmul f3, f1, f2
	fli f4, 0.600000
	fmul f3, f3, f4
	fmul f4, f1, f3
	fli f5, 0.714286
	fmul f4, f4, f5
	fmul f5, f1, f4
	fli f6, 0.777778
	fmul f5, f5, f6
	fmul f1, f1, f5
	fli f6, 0.818182
	fmul f1, f1, f6
	fsub f0, f0, f2
	fadd f0, f0, f3
	fsub f0, f0, f4
	fadd f0, f0, f5
	fsub f0, f0, f1
	jalr zero, ra, 0 # ret
atan:
	fli f1, 0.000000
	fle a20, f1, f0
	beq a20, zero, fble_else.324
	fli f1, 1.000000
	fle a20, f0, f1
	beq a20, zero, fble_else.325
	fli f1, 0.414214
	fle a20, f0, f1
	beq a20, zero, fble_else.326
	jump tailor_atan.135
fble_else.326:
	fli f1, 3.141593
	fli f2, 4.000000
	fdiv f1, f1, f2
	fli f2, 1.000000
	fsub f2, f0, f2
	fli f3, 1.000000
	fadd f0, f3, f0
	fdiv f0, f2, f0
	fsw f1, 0(sp)
	sw ra, -2(sp)
	addi sp, sp, -3
	jal ra, atan # call
	addi sp, sp, 3
	lw ra, -2(sp)
	flw f1, 0(sp)
	fadd f0, f1, f0
	jalr zero, ra, 0 # ret
fble_else.325:
	fli f1, 3.141593
	fli f2, 2.000000
	fdiv f1, f1, f2
	fli f2, 1.000000
	fdiv f0, f2, f0
	fsw f1, -2(sp)
	sw ra, -4(sp)
	addi sp, sp, -5
	jal ra, atan # call
	addi sp, sp, 5
	lw ra, -4(sp)
	flw f1, -2(sp)
	fsub f0, f1, f0
	jalr zero, ra, 0 # ret
fble_else.324:
	fsub f0, zero, f0
	sw ra, -4(sp)
	addi sp, sp, -5
	jal ra, atan # call
	addi sp, sp, 5
	lw ra, -4(sp)
	fsub f0, zero, f0
	jalr zero, ra, 0 # ret