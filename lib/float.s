# fiszero:
#     feq a0, f0, fzero
#     jalr zero, ra, 0
# fispos:
#     flt a0, fzero, f0
#     jalr zero, ra, 0
# fisneg:
#     flt a0, f0, fzero
#     jalr zero, ra, 0
# fneg:
#     fneg f0, f0
#     jalr zero, ra, 0
# fabs:
#     flt a1, f0, fzero
#     bne a1, zero, fabs_l1
#     jalr zero, ra, 0
# fabs_l1:
#     fneg f0, f0
#     jalr zero, ra, 0
# fless:
#     flt a0, f0, f1
#     jalr zero, ra, 0
fhalf:
    lui.float f1, 0.5; addi.float f1, f1, 0.5; nop; nop;
    ret; fmul f0, f0, f1; nop; nop;
# floor:
#     ftoi a0,f0
#     itof f1,a0
#     flt a1,f0,f1
#     sub a0,a0,a1
#     itof f0,a0
#     jalr zero, ra, 0
# int_of_float:
#     ftoi a0, f0
#     jalr zero, ra, 0
# float_of_int:
#     itof f0, a0
#     jalr zero, ra, 0
# sqrt:
#     fsqrt f0, f0
#     jalr zero, ra, 0
# fsqr:
#     fmul f0, f0, f0
#     jalr zero, ra, 0