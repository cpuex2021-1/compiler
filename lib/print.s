print_char:
	ret; nop; sw a0, 0(zero); nop;
print_int:
	li a1, 48; li a2, 48; nop; nop;
	li a3, 48; li a4, 100; nop; nop;
	li a5, 10; li a6, 1; nop; nop;
	li a7, 48; nop; nop; nop;
print_int_l1:
	blt a0, a4, print_int_l2; addi a1, a1, 1; nop; nop;
	jump print_int_l1; addi a0, a0, -100; nop; nop;
print_int_l2:
	blt a0, a5, print_int_l3; addi a2, a2, 1; nop; nop;
	jump print_int_l2; addi a0, a0, -10; nop; nop;
print_int_l3:
	blt a0, a6, print_int_l4; addi a3, a3, 1; nop; nop;
	jump print_int_l3; addi a0, a0, -1; nop; nop;
print_int_l4:
	nop; nop; sw a1, 0(zero); nop;
	nop; nop; sw a2, 0(zero); nop;
	ret; nop; sw a3, 0(zero); nop;