create_array: # a0-length array with value a1
    slli a3, a0, 2
create_array_loop:
    bge zero, a0, create_array_exit
create_array_cont:
    addi a0, a0, -1
    slli a2, a0, 2
    add a2, a2, hp
    sw a1, 0(a2)
    jump create_array_loop
create_array_exit:
    add a0, hp, zero
    add hp, hp, a3
    jalr zero, ra, 0
create_float_array: # a0-length array with value f0
    slli a3, a0, 2
create_float_array_loop:
    bge zero, a0, create_float_array_exit
create_float_array_cont:
    addi a0, a0, -1
    slli a2, a0, 2
    add a2, a2, hp
    sw f0, 0(a2)
    jump create_float_array_loop
create_float_array_exit:
    add a0, hp, zero
    add hp, hp, a3
    jalr zero, ra, 0