#pi:ft0, s3/c2:ft1, s5/c4:ft2, s7/c6:ft3 2:ft5 4:ft6
#A:fa0, FLAG:a0, 0:fa1, P:fa2, pi*2:fa3 pi/2:fa4 pi/4:fa5
set_param:
    lui t0, 263312
    addi t0, t0, 4059
    lui t5, 262144
    lui t6, 264192
    fmv.w.x ft0, t0
    fmv.w.x ft5, t5
    fmv.w.x ft6, t6
flag:
    fmv.w.x fa1, zero
    flt a0, fa1, fa0
    beq a0, zero, red1
neg:
    fneg fa0, fa0
red1:
    fmul fa2, ft0, ft5
red1_1_loop:
    fle a1, fa2, fa0
    beq a1, zero, red1_1_loop_end
    fmul fa2, fa2, ft5
    jump red1_1_loop
red1_1_loop_end:
    fmul fa3, ft0, ft5
red1_2_loop:
    fle a1, fa3, fa0
    beq a1, zero, red1_end
    fle a1, fa2, fa0
    beq a1, zero, red1_else
    fsub fa0, fa0, fa2
red1_else:
    fdiv fa2, fa2, ft5
    jump red1_2_loop
red1_end:
red2:
    fle a1, ft0, fa0
    beq a1, zero, red2_br2
    fsub fa0, fa0, ft0
    addi a2, zero, 1
    sub a0, a2, a0
red2_br2:
    fdiv fa4, ft0, ft5
    fle a1, fa4, fa0
    beq a1, zero, red2_br3
    fsub fa0, ft0, fa0
red2_br3:
    fdiv fa5, ft0, ft5
    fle a1, fa0, fa5
    beq a1, zero, red2_else
    jal ra, kernel_sin
    jump red2_end
red2_else: 
    fsub fa0, fa4, fa0
    jal ra, kernel_cos
red2_end:
    flt a2, fa0, fa1
    beq a2, a0, end
    fneg fa0, fa0
kernel_sin:
    lui t1, 254634
    addi t1, t1, 2732
    lui t2, 245896
    addi t2, t2, 1638
    lui t3, 758998
    addi t3, t3, 1206
    fmv.w.x ft1, t1
    fmv.w.x ft2, t2
    fmv.w.x ft3, t3
    fmul ft10, fa0, fa0
    fmul ft10, ft10, fa0
    fmul ft10, ft1, ft10
    fsub ft10, fa0, ft10
    fmul ft11, fa0, fa0
    fmul ft11, ft11, ft11
    fmul ft11, fa0, ft11
    fmul ft11, ft11, ft2
    fadd ft10, ft10, ft11
    fmul ft11, fa0, fa0
    fmul ft11, ft11, ft11
    fmul ft11, fa0, ft11
    fmul ft11, fa0, ft11
    fmul ft11, fa0, ft11
    fmul ft11, ft11, ft3 
    fsub fa0, ft10, ft11
    jalr zero, ra, 0
kernel_cos:
    lui t2, 250538
    addi t2, t2, 1929
    lui t3, 240440
    addi t3, t3, 262
    fmv.w.x ft2, t2
    fmv.w.x ft3, t3
    fmul ft11, fa0, fa0
    fdiv ft11, ft11, ft5
    fdiv ft10, ft5, ft5
    fsub ft10, ft10, ft11
    fmul ft11, ft11, ft11
    fmul ft11, ft11, ft2
    fadd ft10, ft11, ft10
    fmul ft11, ft11, fa0
    fmul ft11, ft11, fa0
    fmul ft11, ft11, ft3
    fsub fa0, ft10, ft11
    jalr zero, ra, 0
end: