ADDI X2 X0 1
outer:
BEQ X2 X0 end
ADDI X2 X0 0
ADDI X3 X0 1
inner:
BEQ X3 X1 innerend
LW X4 X3 -1
LW X5 X3 0
BLT X5 X4 swap
afterswap:
ADDI X3 X3 1
BEQ X0 X0 inner
innerend:
BEQ X0 X0 outer
end:
HALT
swap:
SW X4 X3 0
SW X5 X3 -1
ADDI X2 X0 1
BEQ X0 X0 afterswap