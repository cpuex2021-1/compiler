create_array: # a0-length array with value a1
    addi a3, a0, 0; nop; nop; nop;
create_array_loop:
    bge zero, a0, create_array_exit; nop; nop; nop;
create_array_cont:
    addi a0, a0, -1; nop; nop; nop;
    add a2, a0, hp; nop; nop; nop;
    jump create_array_loop; nop; sw a1, 0(a2); nop;
create_array_exit:
    add a0, hp, zero; add hp, hp, a3; nop; nop;
    ret; nop; nop; nop;
create_float_array: # a0-length array with value f0
    addi a3, a0, 0; nop; nop; nop;
create_float_array_loop:
    bge zero, a0, create_float_array_exit; nop; nop; nop;
create_float_array_cont:
    addi a0, a0, -1; nop; nop; nop;
    add a2, a0, hp; nop; nop; nop;
    jump create_float_array_loop; nop; sw f0, 0(a2); nop;
create_float_array_exit:
    add a0, hp, zero; add hp, hp, a3; nop; nop;
    ret; nop; nop; nop;
create_global_array: # a1-length array with value a2 @address a0
    addi a3, a0, 0; addi a0, a1, 0; nop; nop;
    addi a1, a2, 0; nop; nop; nop;
create_global_array_loop:
    bge zero, a0, create_global_array_exit; nop; nop; nop;
create_global_array_cont:
    addi a0, a0, -1; nop; nop; nop;
    add a4, a0, a3; nop; nop; nop;
    jump create_global_array_loop; nop; sw a1, 0(a4); nop;
create_global_array_exit:
    ret; add a0, a3, zero; nop; nop;
create_global_float_array: # a1-length array with value f0 @address a0
    addi a3, a0, 0; addi a0, a1, 0; nop; nop;
create_global_float_array_loop:
    bge zero, a0, create_global_float_array_exit; nop; nop; nop;
create_global_float_array_cont:
    addi a0, a0, -1; nop; nop; nop;
    add a4, a0, a3; nop; nop; nop;
    jump create_global_float_array_loop; nop; sw f0, 0(a4); nop;
create_global_float_array_exit:
    ret; add a0, a3, zero; nop; nop;