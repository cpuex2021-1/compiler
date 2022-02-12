
pi_div.124:
	lui.float f2, 0.000000; addi.float f2, f2, 0.000000; nop; nop; 
	nop; feq a20, f2, f0; nop; nop; 
	beq a20, zero, fbe_else.314; nop; nop; nop; 
	ret; nop; nop; nop; 
	nop; nop; nop; nop; 
fbe_else.314:
	lui.float f2, 0.000000; addi.float f2, f2, 0.000000; nop; nop; 
	nop; fle a20, f0, f2; nop; nop; 
	beq a20, zero, fble_else.315; fneg f2, f0; nop; nop; 
	nop; fle a20, f2, f1; nop; nop; 
	beq a20, zero, fble_else.316; fadd f0, f0, f1; nop; nop; 
	lui.float f2, 0.500000; addi.float f2, f2, 0.500000; nop; nop; 
	jump pi_div.124; fmul f1, f1, f2; nop; nop; 
	nop; nop; nop; nop; 
fble_else.316:
	lui.float f2, 2.000000; addi.float f2, f2, 2.000000; nop; nop; 
	jump pi_div.124; fmul f1, f1, f2; nop; nop; 
	nop; nop; nop; nop; 
fble_else.315:
	lui.float f2, 3.141593; addi.float f2, f2, 3.141593; nop; nop; 
	lui.float f3, 2.000000; addi.float f3, f3, 2.000000; nop; nop; 
	nop; fmul f2, f2, f3; nop; nop; 
	nop; fle a20, f2, f0; nop; nop; 
	beq a20, zero, fble_else.317; fle a20, f0, f1; nop; nop; 
	beq a20, zero, fble_else.318; addi.float f2, f2, 0.500000; nop; nop; 
	nop; lui.float f2, 0.500000; nop; nop; 
	nop; fmul f2, f1, f2; nop; nop; 
	addi.float f2, f2, 0.500000; fsub f0, f0, f2; nop; nop; 
	nop; lui.float f2, 0.500000; nop; nop; 
	jump pi_div.124; fmul f1, f1, f2; nop; nop; 
	nop; nop; nop; nop; 
fble_else.318:
	lui.float f2, 2.000000; addi.float f2, f2, 2.000000; nop; nop; 
	jump pi_div.124; fmul f1, f1, f2; nop; nop; 
	nop; nop; nop; nop; 
fble_else.317:
	ret; nop; nop; nop; 
	nop; nop; nop; nop; 
pi4div.127:
	lui.float f1, 3.141593; addi.float f1, f1, 3.141593; nop; nop; 
	lui.float f2, 2.000000; addi.float f2, f2, 2.000000; nop; nop; 
	nop; fdiv f1, f1, f2; nop; nop; 
	nop; fle a20, f1, f0; nop; nop; 
	beq a20, zero, fble_else.319; addi.float f1, f1, 3.141593; nop; nop; 
	nop; lui.float f1, 3.141593; nop; nop; 
	nop; fle a20, f1, f0; nop; nop; 
	beq a20, zero, fble_else.320; addi.float f1, f1, 3.141593; nop; nop; 
	addi.float f2, f2, 1.500000; lui.float f1, 3.141593; nop; nop; 
	nop; lui.float f2, 1.500000; nop; nop; 
	nop; fmul f1, f1, f2; nop; nop; 
	nop; fle a20, f1, f0; nop; nop; 
	beq a20, zero, fble_else.321; addi.float f1, f1, 3.141593; nop; nop; 
	addi.float f2, f2, 2.000000; lui.float f1, 3.141593; nop; nop; 
	nop; lui.float f2, 2.000000; nop; nop; 
	nop; fmul f1, f1, f2; nop; nop; 
	addi.float f1, f1, 1.000000; fsub f0, f1, f0; nop; nop; 
	add a0, hp, zero; lui.float f1, 1.000000; nop; nop; 
	ret; addi hp, hp, 8; sw f1, 4(a0); sw f0, 0(a0); 
	nop; nop; nop; nop; 
fble_else.321:
	lui.float f1, 3.141593; addi.float f1, f1, 3.141593; nop; nop; 
	addi.float f1, f1, -1.000000; fsub f0, f0, f1; nop; nop; 
	add a0, hp, zero; lui.float f1, -1.000000; nop; nop; 
	ret; addi hp, hp, 8; sw f1, 4(a0); sw f0, 0(a0); 
	nop; nop; nop; nop; 
fble_else.320:
	lui.float f1, 3.141593; addi.float f1, f1, 3.141593; nop; nop; 
	addi.float f1, f1, -1.000000; fsub f0, f1, f0; nop; nop; 
	add a0, hp, zero; lui.float f1, -1.000000; nop; nop; 
	ret; addi hp, hp, 8; sw f1, 4(a0); sw f0, 0(a0); 
	nop; nop; nop; nop; 
fble_else.319:
	lui.float f1, 1.000000; addi.float f1, f1, 1.000000; nop; nop; 
	addi hp, hp, 8; add a0, hp, zero; nop; nop; 
	ret; nop; sw f1, 4(a0); sw f0, 0(a0); 
	nop; nop; nop; nop; 
pi4div2.129:
	lui.float f1, 3.141593; addi.float f1, f1, 3.141593; nop; nop; 
	lui.float f2, 2.000000; addi.float f2, f2, 2.000000; nop; nop; 
	nop; fdiv f1, f1, f2; nop; nop; 
	nop; fle a20, f1, f0; nop; nop; 
	beq a20, zero, fble_else.322; addi.float f1, f1, 3.141593; nop; nop; 
	nop; lui.float f1, 3.141593; nop; nop; 
	nop; fle a20, f1, f0; nop; nop; 
	beq a20, zero, fble_else.323; addi.float f1, f1, 3.141593; nop; nop; 
	addi.float f2, f2, 1.500000; lui.float f1, 3.141593; nop; nop; 
	nop; lui.float f2, 1.500000; nop; nop; 
	nop; fmul f1, f1, f2; nop; nop; 
	nop; fle a20, f1, f0; nop; nop; 
	beq a20, zero, fble_else.324; addi.float f1, f1, 3.141593; nop; nop; 
	addi.float f2, f2, 2.000000; lui.float f1, 3.141593; nop; nop; 
	nop; lui.float f2, 2.000000; nop; nop; 
	nop; fmul f1, f1, f2; nop; nop; 
	addi.float f1, f1, -1.000000; fsub f0, f1, f0; nop; nop; 
	add a0, hp, zero; lui.float f1, -1.000000; nop; nop; 
	ret; addi hp, hp, 8; sw f1, 4(a0); sw f0, 0(a0); 
	nop; nop; nop; nop; 
fble_else.324:
	lui.float f1, 3.141593; addi.float f1, f1, 3.141593; nop; nop; 
	addi.float f1, f1, -1.000000; fsub f0, f0, f1; nop; nop; 
	add a0, hp, zero; lui.float f1, -1.000000; nop; nop; 
	ret; addi hp, hp, 8; sw f1, 4(a0); sw f0, 0(a0); 
	nop; nop; nop; nop; 
fble_else.323:
	lui.float f1, 3.141593; addi.float f1, f1, 3.141593; nop; nop; 
	addi.float f1, f1, 1.000000; fsub f0, f1, f0; nop; nop; 
	add a0, hp, zero; lui.float f1, 1.000000; nop; nop; 
	ret; addi hp, hp, 8; sw f1, 4(a0); sw f0, 0(a0); 
	nop; nop; nop; nop; 
fble_else.322:
	lui.float f1, 1.000000; addi.float f1, f1, 1.000000; nop; nop; 
	addi hp, hp, 8; add a0, hp, zero; nop; nop; 
	ret; nop; sw f1, 4(a0); sw f0, 0(a0); 
	nop; nop; nop; nop; 
tailor_cos1.131:
	addi.float f1, f1, 0.500000; fmul f0, f0, f0; nop; nop; 
	nop; lui.float f1, 0.500000; nop; nop; 
	nop; fmul f1, f0, f1; nop; nop; 
	addi.float f3, f3, 0.083333; fmul f2, f0, f1; nop; nop; 
	nop; lui.float f3, 0.083333; nop; nop; 
	nop; fmul f2, f2, f3; nop; nop; 
	addi.float f4, f4, 0.033333; fmul f3, f0, f2; nop; nop; 
	nop; lui.float f4, 0.033333; nop; nop; 
	nop; fmul f3, f3, f4; nop; nop; 
	addi.float f5, f5, 0.017857; fmul f4, f0, f3; nop; nop; 
	nop; lui.float f5, 0.017857; nop; nop; 
	nop; fmul f4, f4, f5; nop; nop; 
	addi.float f6, f6, 0.011111; fmul f5, f0, f4; nop; nop; 
	nop; lui.float f6, 0.011111; nop; nop; 
	nop; fmul f5, f5, f6; nop; nop; 
	addi.float f6, f6, 0.007576; fmul f0, f0, f5; nop; nop; 
	nop; lui.float f6, 0.007576; nop; nop; 
	addi.float f6, f6, 1.000000; fmul f0, f0, f6; nop; nop; 
	nop; lui.float f6, 1.000000; nop; nop; 
	nop; fsub f1, f6, f1; nop; nop; 
	nop; fadd f1, f1, f2; nop; nop; 
	nop; fsub f1, f1, f3; nop; nop; 
	nop; fadd f1, f1, f4; nop; nop; 
	nop; fsub f1, f1, f5; nop; nop; 
	ret; fadd f0, f1, f0; nop; nop; 
	nop; nop; nop; nop; 
cos:
	lui.float f1, 3.141593; addi.float f1, f1, 3.141593; nop; nop; 
	lui.float f2, 2.000000; addi.float f2, f2, 2.000000; nop; nop; 
	addi sp, sp, -1; fmul f1, f1, f2; nop; nop; 
	call pi_div.124; nop; nop; nop; 
	call pi4div.127; nop; nop; nop; 
	nop; addi sp, sp, 1; lw f1, 0(a0); lw f0, 4(a0); 
	addi sp, sp, -3; add f0, f1, zero; sw f0, 0(sp); nop; 
	call tailor_cos1.131; nop; nop; nop; 
	nop; addi sp, sp, 3; nop; nop; 
	nop; nop; lw f1, 0(sp); nop; 
	ret; fmul f0, f1, f0; nop; nop; 
	nop; nop; nop; nop; 
sin:
	lui.float f1, 3.141593; addi.float f1, f1, 3.141593; nop; nop; 
	lui.float f2, 2.000000; addi.float f2, f2, 2.000000; nop; nop; 
	addi sp, sp, -1; fmul f1, f1, f2; nop; nop; 
	call pi_div.124; nop; nop; nop; 
	call pi4div2.129; nop; nop; nop; 
	addi.float f2, f2, 3.141593; addi sp, sp, 1; lw f1, 0(a0); lw f0, 4(a0); 
	addi.float f3, f3, 2.000000; lui.float f2, 3.141593; nop; nop; 
	nop; lui.float f3, 2.000000; nop; nop; 
	nop; fdiv f2, f2, f3; nop; nop; 
	nop; fsub f1, f2, f1; sw f0, 0(sp); nop; 
	addi sp, sp, -3; add f0, f1, zero; nop; nop; 
	call tailor_cos1.131; nop; nop; nop; 
	nop; addi sp, sp, 3; nop; nop; 
	nop; nop; lw f1, 0(sp); nop; 
	ret; fmul f0, f1, f0; nop; nop; 
	nop; nop; nop; nop; 
tailor_atan1.137:
	nop; fmul f1, f0, f0; nop; nop; 
	addi.float f3, f3, 0.333333; fmul f2, f1, f0; nop; nop; 
	nop; lui.float f3, 0.333333; nop; nop; 
	nop; fmul f2, f2, f3; nop; nop; 
	addi.float f4, f4, 0.600000; fmul f3, f1, f2; nop; nop; 
	nop; lui.float f4, 0.600000; nop; nop; 
	nop; fmul f3, f3, f4; nop; nop; 
	addi.float f5, f5, 0.714286; fmul f4, f1, f3; nop; nop; 
	nop; lui.float f5, 0.714286; nop; nop; 
	nop; fmul f4, f4, f5; nop; nop; 
	addi.float f6, f6, 0.777778; fmul f5, f1, f4; nop; nop; 
	nop; lui.float f6, 0.777778; nop; nop; 
	nop; fmul f5, f5, f6; nop; nop; 
	addi.float f6, f6, 0.818182; fmul f1, f1, f5; nop; nop; 
	nop; lui.float f6, 0.818182; nop; nop; 
	fsub f0, f0, f2; fmul f1, f1, f6; nop; nop; 
	nop; fadd f0, f0, f3; nop; nop; 
	nop; fsub f0, f0, f4; nop; nop; 
	nop; fadd f0, f0, f5; nop; nop; 
	ret; fsub f0, f0, f1; nop; nop; 
	nop; nop; nop; nop; 
atan:
	lui.float f1, 0.000000; addi.float f1, f1, 0.000000; nop; nop; 
	nop; fle a20, f1, f0; nop; nop; 
	beq a20, zero, fble_else.325; addi.float f1, f1, 1.000000; nop; nop; 
	nop; lui.float f1, 1.000000; nop; nop; 
	nop; fle a20, f0, f1; nop; nop; 
	beq a20, zero, fble_else.326; addi.float f1, f1, 0.414214; nop; nop; 
	nop; lui.float f1, 0.414214; nop; nop; 
	nop; fle a20, f0, f1; nop; nop; 
	beq a20, zero, fble_else.327; nop; nop; nop; 
	jump tailor_atan1.137; nop; nop; nop; 
	nop; nop; nop; nop; 
fble_else.327:
	lui.float f1, 3.141593; addi.float f1, f1, 3.141593; nop; nop; 
	lui.float f2, 4.000000; addi.float f2, f2, 4.000000; nop; nop; 
	addi.float f2, f2, 1.000000; fdiv f1, f1, f2; nop; nop; 
	nop; lui.float f2, 1.000000; nop; nop; 
	addi.float f3, f3, 1.000000; fsub f2, f0, f2; nop; nop; 
	nop; lui.float f3, 1.000000; nop; nop; 
	nop; fadd f0, f3, f0; nop; nop; 
	addi sp, sp, -3; fdiv f0, f2, f0; sw f1, 0(sp); nop; 
	call atan; nop; nop; nop; 
	nop; addi sp, sp, 3; nop; nop; 
	nop; nop; lw f1, 0(sp); nop; 
	ret; fadd f0, f1, f0; nop; nop; 
	nop; nop; nop; nop; 
fble_else.326:
	lui.float f1, 3.141593; addi.float f1, f1, 3.141593; nop; nop; 
	lui.float f2, 2.000000; addi.float f2, f2, 2.000000; nop; nop; 
	addi.float f2, f2, 1.000000; fdiv f1, f1, f2; nop; nop; 
	nop; lui.float f2, 1.000000; nop; nop; 
	addi sp, sp, -3; fdiv f0, f2, f0; sw f1, -1(sp); nop; 
	call atan; nop; nop; nop; 
	nop; addi sp, sp, 3; nop; nop; 
	nop; nop; lw f1, -1(sp); nop; 
	ret; fsub f0, f1, f0; nop; nop; 
	nop; nop; nop; nop; 
fble_else.325:
	addi sp, sp, -3; fneg f0, f0; nop; nop; 
	call atan; nop; nop; nop; 
	fneg f0, f0; addi sp, sp, 3; nop; nop; 
	ret; nop; nop; nop; 
	nop; nop; nop; nop; 