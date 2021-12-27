fiszero:
    feq a0, f0, fzero
    jalr zero, ra, 0
fispos:
    flt a0, fzero, f0
    jalr zero, ra, 0
fisneg:
    flt a0, f0, fzero
    jalr zero, ra, 0
fneg:
    fneg f0, f0
    jalr zero, ra, 0
fabs:
    flt a1, f0, fzero
    bne fabs_l1, zero, fabs_l1
    jalr zero, ra, 0
fabs_l1:
    fli f1, -1
    fmul f0, f0, f1
    jalr zero, ra, 0
fless:
    flt a0, f0, f1
    jalr zero, ra, 0
fhalf:
    fli f1, 0.5
    fmul f0, f0, f1
    jalr zero, ra, 0
floor:
    ftoi a0, f0
    itof f0, a0
    jalr zero, ra, 0
int_of_float:
    ftoi a0, f0
    jalr zero, ra, 0
float_of_int:
    itof f0, a0
    jalr zero, ra, 0
sqrt: # what's the difference of sqrt and fsqr?
    fsqrt f0, f0
    jalr zero, ra, 0
fsqr:
    fsqrt f0, f0
    jalr zero, ra, 0