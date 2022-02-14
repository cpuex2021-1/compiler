create_array: # a0-length array with value a1
    addi a3, a0, 0
create_array_loop:
    bge zero, a0, create_array_exit
create_array_cont:
    addi a0, a0, -1
    # slli a2, a0, 2
    add a2, a0, hp
    sw a1, 0(a2)
    jump create_array_loop
create_array_exit:
    add a0, hp, zero
    add hp, hp, a3
    ret
create_float_array: # a0-length array with value f0
    addi a3, a0, 0
create_float_array_loop:
    bge zero, a0, create_float_array_exit
create_float_array_cont:
    addi a0, a0, -1
    # slli a2, a0, 2
    add a2, a0, hp
    sw f0, 0(a2)
    jump create_float_array_loop
create_float_array_exit:
    add a0, hp, zero
    add hp, hp, a3
    ret
create_global_array: # a1-length array with value a2 @address a0
    addi a3, a0, 0
    addi a0, a1, 0
    addi a1, a2, 0
create_global_array_loop:
    bge zero, a0, create_global_array_exit
create_global_array_cont:
    addi a0, a0, -1
    add a4, a0, a3
    sw a1, 0(a4)
    jump create_global_array_loop
create_global_array_exit:
    add a0, a3, zero
    ret
create_global_float_array: # a1-length array with value f0 @address a0
    addi a3, a0, 0
    addi a0, a1, 0
create_global_float_array_loop:
    bge zero, a0, create_global_float_array_exit
create_global_float_array_cont:
    addi a0, a0, -1
    add a4, a0, a3
    sw f0, 0(a4)
    jump create_global_float_array_loop
create_global_float_array_exit:
    add a0, a3, zero
    ret