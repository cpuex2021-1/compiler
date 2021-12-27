read_int:
    lw a0, 0(zero)
	jalr zero, ra, 0
read_float:
    lw f0, 0(zero)
	jalr zero, ra, 0