read_int:
    nop; nop; lw a0, 0(zero); nop;
    nop; nop; lw a1, 0(zero); nop;
    slli a1, a1, 8; nop; nop; nop;
    add a0, a0, a1; nop; lw a1, 0(zero); nop;
    slli a1, a1, 16; nop; nop; nop;
    add a0, a0, a1; nop; lw a1 , 0(zero); nop;
    slli a1, a1, 24; nop; nop; nop;
    ret; add a0, a0, a1; nop; nop;
read_float:
    nop; nop; lw a0, 0(zero); nop;
    nop; nop; lw a1, 0(zero); nop;
    slli a1, a1, 8; nop; nop; nop;
    add a0, a0, a1; nop; lw a1, 0(zero); nop;
    slli a1, a1, 16; nop; nop; nop;
    add a0, a0, a1; nop; lw a1 , 0(zero); nop;
    slli a1, a1, 24; nop; nop; nop;
    add a0, a0, a1; nop; nop; nop;
    ret; fmv.w.x f0, a0; nop; nop;