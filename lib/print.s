print_char:
    sw a0, 0(zero)
	ret
print_int:
	li a1, 48
	li a2, 48
	li a3, 48
	li a4, 100
	li a5, 10
	li a6, 1
	li a7, 48
print_int_l1:
	blt a0, a4, print_int_l2
	addi a1, a1, 1
	addi a0, a0, -100
	jump print_int_l1
print_int_l2:
	blt a0, a5, print_int_l3
	addi a2, a2, 1
	addi a0, a0, -10
	jump print_int_l2
print_int_l3:
	blt a0, a6, print_int_l4
	addi a3, a3, 1
	addi a0, a0, -1
	jump print_int_l3
print_int_l4:
	sw a1, 0(zero)
	sw a2, 0(zero)
	sw a3, 0(zero)
	ret