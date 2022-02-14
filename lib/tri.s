pi_div.124:
	addi.float f2, f2, 0.000000
	lui.float f2, 0.000000
	feq a20, f2, f0
	beq a20, zero, fbe_else.314
	ret
fbe_else.314:
	addi.float f2, f2, 0.000000
	lui.float f2, 0.000000
	fle a20, f0, f2
	beq a20, zero, fble_else.315
	fneg f2, f0
	fle a20, f2, f1
	beq a20, zero, fble_else.316
	fadd f0, f0, f1
	addi.float f2, f2, 0.500000
	lui.float f2, 0.500000
	fmul f1, f1, f2
	jump pi_div.124
fble_else.316:
	addi.float f2, f2, 2.000000
	lui.float f2, 2.000000
	fmul f1, f1, f2
	jump pi_div.124
fble_else.315:
	addi.float f2, f2, 3.141593
	lui.float f2, 3.141593
	addi.float f3, f3, 2.000000
	lui.float f3, 2.000000
	fmul f2, f2, f3
	fle a20, f2, f0
	beq a20, zero, fble_else.317
	fle a20, f0, f1
	beq a20, zero, fble_else.318
	addi.float f2, f2, 0.500000
	lui.float f2, 0.500000
	fmul f2, f1, f2
	fsub f0, f0, f2
	addi.float f2, f2, 0.500000
	lui.float f2, 0.500000
	fmul f1, f1, f2
	jump pi_div.124
fble_else.318:
	addi.float f2, f2, 2.000000
	lui.float f2, 2.000000
	fmul f1, f1, f2
	jump pi_div.124
fble_else.317:
	ret
pi4div.127:
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	addi.float f2, f2, 2.000000
	lui.float f2, 2.000000
	fdiv f1, f1, f2
	fle a20, f1, f0
	beq a20, zero, fble_else.319
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	fle a20, f1, f0
	beq a20, zero, fble_else.320
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	addi.float f2, f2, 1.500000
	lui.float f2, 1.500000
	fmul f1, f1, f2
	fle a20, f1, f0
	beq a20, zero, fble_else.321
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	addi.float f2, f2, 2.000000
	lui.float f2, 2.000000
	fmul f1, f1, f2
	fsub f0, f1, f0
	addi.float f1, f1, 1.000000
	lui.float f1, 1.000000
	add a0, hp, zero
	addi hp, hp, 8
	sw f1, 4(a0)
	sw f0, 0(a0)
	ret
fble_else.321:
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	fsub f0, f0, f1
	addi.float f1, f1, -1.000000
	lui.float f1, -1.000000
	add a0, hp, zero
	addi hp, hp, 8
	sw f1, 4(a0)
	sw f0, 0(a0)
	ret
fble_else.320:
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	fsub f0, f1, f0
	addi.float f1, f1, -1.000000
	lui.float f1, -1.000000
	add a0, hp, zero
	addi hp, hp, 8
	sw f1, 4(a0)
	sw f0, 0(a0)
	ret
fble_else.319:
	addi.float f1, f1, 1.000000
	lui.float f1, 1.000000
	add a0, hp, zero
	addi hp, hp, 8
	sw f1, 4(a0)
	sw f0, 0(a0)
	ret
pi4div2.129:
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	addi.float f2, f2, 2.000000
	lui.float f2, 2.000000
	fdiv f1, f1, f2
	fle a20, f1, f0
	beq a20, zero, fble_else.322
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	fle a20, f1, f0
	beq a20, zero, fble_else.323
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	addi.float f2, f2, 1.500000
	lui.float f2, 1.500000
	fmul f1, f1, f2
	fle a20, f1, f0
	beq a20, zero, fble_else.324
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	addi.float f2, f2, 2.000000
	lui.float f2, 2.000000
	fmul f1, f1, f2
	fsub f0, f1, f0
	addi.float f1, f1, -1.000000
	lui.float f1, -1.000000
	add a0, hp, zero
	addi hp, hp, 8
	sw f1, 4(a0)
	sw f0, 0(a0)
	ret
fble_else.324:
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	fsub f0, f0, f1
	addi.float f1, f1, -1.000000
	lui.float f1, -1.000000
	add a0, hp, zero
	addi hp, hp, 8
	sw f1, 4(a0)
	sw f0, 0(a0)
	ret
fble_else.323:
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	fsub f0, f1, f0
	addi.float f1, f1, 1.000000
	lui.float f1, 1.000000
	add a0, hp, zero
	addi hp, hp, 8
	sw f1, 4(a0)
	sw f0, 0(a0)
	ret
fble_else.322:
	addi.float f1, f1, 1.000000
	lui.float f1, 1.000000
	add a0, hp, zero
	addi hp, hp, 8
	sw f1, 4(a0)
	sw f0, 0(a0)
	ret
tailor_cos1.131:
	fmul f0, f0, f0
	addi.float f1, f1, 0.500000
	lui.float f1, 0.500000
	fmul f1, f0, f1
	fmul f2, f0, f1
	addi.float f3, f3, 0.083333
	lui.float f3, 0.083333
	fmul f2, f2, f3
	fmul f3, f0, f2
	addi.float f4, f4, 0.033333
	lui.float f4, 0.033333
	fmul f3, f3, f4
	fmul f4, f0, f3
	addi.float f5, f5, 0.017857
	lui.float f5, 0.017857
	fmul f4, f4, f5
	fmul f5, f0, f4
	addi.float f6, f6, 0.011111
	lui.float f6, 0.011111
	fmul f5, f5, f6
	fmul f0, f0, f5
	addi.float f6, f6, 0.007576
	lui.float f6, 0.007576
	fmul f0, f0, f6
	addi.float f6, f6, 1.000000
	lui.float f6, 1.000000
	fsub f1, f6, f1
	fadd f1, f1, f2
	fsub f1, f1, f3
	fadd f1, f1, f4
	fsub f1, f1, f5
	fadd f0, f1, f0
	ret
cos:
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	addi.float f2, f2, 2.000000
	lui.float f2, 2.000000
	fmul f1, f1, f2
	addi sp, sp, -1
	call pi_div.124
	call pi4div.127
	lw f1, 0(a0)
	lw f0, 4(a0)
	addi sp, sp, 1
	sw f0, 0(sp)
	add f0, f1, zero
	addi sp, sp, -3
	call tailor_cos1.131
	addi sp, sp, 3
	lw f1, 0(sp)
	fmul f0, f1, f0
	ret
sin:
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	addi.float f2, f2, 2.000000
	lui.float f2, 2.000000
	fmul f1, f1, f2
	addi sp, sp, -1
	call pi_div.124
	call pi4div2.129
	lw f1, 0(a0)
	lw f0, 4(a0)
	addi sp, sp, 1
	addi.float f2, f2, 3.141593
	lui.float f2, 3.141593
	addi.float f3, f3, 2.000000
	lui.float f3, 2.000000
	fdiv f2, f2, f3
	fsub f1, f2, f1
	sw f0, 0(sp)
	add f0, f1, zero
	addi sp, sp, -3
	call tailor_cos1.131
	addi sp, sp, 3
	lw f1, 0(sp)
	fmul f0, f1, f0
	ret
tailor_atan1.137:
	fmul f1, f0, f0
	fmul f2, f1, f0
	addi.float f3, f3, 0.333333
	lui.float f3, 0.333333
	fmul f2, f2, f3
	fmul f3, f1, f2
	addi.float f4, f4, 0.600000
	lui.float f4, 0.600000
	fmul f3, f3, f4
	fmul f4, f1, f3
	addi.float f5, f5, 0.714286
	lui.float f5, 0.714286
	fmul f4, f4, f5
	fmul f5, f1, f4
	addi.float f6, f6, 0.777778
	lui.float f6, 0.777778
	fmul f5, f5, f6
	fmul f1, f1, f5
	addi.float f6, f6, 0.818182
	lui.float f6, 0.818182
	fmul f1, f1, f6
	fsub f0, f0, f2
	fadd f0, f0, f3
	fsub f0, f0, f4
	fadd f0, f0, f5
	fsub f0, f0, f1
	ret
atan:
	addi.float f1, f1, 0.000000
	lui.float f1, 0.000000
	fle a20, f1, f0
	beq a20, zero, fble_else.325
	addi.float f1, f1, 1.000000
	lui.float f1, 1.000000
	fle a20, f0, f1
	beq a20, zero, fble_else.326
	addi.float f1, f1, 0.414214
	lui.float f1, 0.414214
	fle a20, f0, f1
	beq a20, zero, fble_else.327
	jump tailor_atan1.137
fble_else.327:
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	addi.float f2, f2, 4.000000
	lui.float f2, 4.000000
	fdiv f1, f1, f2
	addi.float f2, f2, 1.000000
	lui.float f2, 1.000000
	fsub f2, f0, f2
	addi.float f3, f3, 1.000000
	lui.float f3, 1.000000
	fadd f0, f3, f0
	fdiv f0, f2, f0
	sw f1, 0(sp)
	addi sp, sp, -3
	call atan
	addi sp, sp, 3
	lw f1, 0(sp)
	fadd f0, f1, f0
	ret
fble_else.326:
	addi.float f1, f1, 3.141593
	lui.float f1, 3.141593
	addi.float f2, f2, 2.000000
	lui.float f2, 2.000000
	fdiv f1, f1, f2
	addi.float f2, f2, 1.000000
	lui.float f2, 1.000000
	fdiv f0, f2, f0
	sw f1, -1(sp)
	addi sp, sp, -3
	call atan
	addi sp, sp, 3
	lw f1, -1(sp)
	fsub f0, f1, f0
	ret
fble_else.325:
	fneg f0, f0
	addi sp, sp, -3
	call atan
	addi sp, sp, 3
	fneg f0, f0
	ret