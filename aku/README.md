# aku

aku is a simulation of a pipelined CPU, inspired by Bruce Jacob's [RiSC-16](https://www.ece.umd.edu/~blj/RiSC/)

## Build and run

1. Install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
2. In the root directory of the project, run `stack build`
3. To run the test suite, run `stack test`

## Features

* Five-stage RISC pipeline
* Operand forwarding