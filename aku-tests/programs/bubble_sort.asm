ADDI X1 X0 16 -- n
ADDI X2 X0 1 -- swapped
outer:
BEQ X2 X0 end
ADDI X3 X0 1 -- i
inner:
BEQ X3 X1 innerend

BEQ X0 X0 inner
innerend:
BEQ X0 X0 outer
end:
HALT