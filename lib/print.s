print_char:
    sw a0, 0(zero)
	jalr zero, ra, 0
print_int:
	sw a0, 0(zero)
	srli a0, a0, 8
	sw a0, 0(zero)
	srli a0, a0, 8
	sw a0, 0(zero)
	srli a0, a0, 8
	sw a0, 0(zero)
	jalr zero, ra, 0