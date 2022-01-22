read_int:
    lw a0, 0(zero)
    lw a1, 0(zero)
    slli a1, a1, 8
    add a0, a0, a1
    lw a1, 0(zero)
    slli a1, a1, 16
    add a0, a0, a1
    lw a1 , 0(zero)
    slli a1, a1, 24
    add a0, a0, a1
    jalr zero, ra, 0
read_float:
    lw a0, 0(zero)
    lw a1, 0(zero)
    slli a1, a1, 8
    add a0, a0, a1
    lw a1, 0(zero)
    slli a1, a1, 16
    add a0, a0, a1
    lw a1 , 0(zero)
    slli a1, a1, 24
    add a0, a0, a1
    fmv.w.x f0, a0
    jalr zero, ra, 0