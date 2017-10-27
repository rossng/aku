{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BinaryLiterals #-}
module SimplePrograms where

import Test.HUnit
import Control.Lens
import Data.Vector as V

import CPU
import Utils
import Instruction
import Registers
import qualified Memory as M

testSimplePrograms = TestList [
          testSimpleProgram1
        , testSimpleProgram2
        , testSimpleProgram3
    ]

testSimpleProgram1 = TestCase $ assertEqual
            "ADD X0 X0 X0, BEQ X2 X3 -2, ADDI X1 X1 5"
            0
            (readRegister regs X1)
        where prog = M.Program [
                        ADD (Dest X0) (Source X0) (Source X0),
                        BEQ (Source X2) (Source X3) (ImmS (-2)),
                        ADDI (Dest X1) (Source X1) (ImmS 5)]
              init = initialCPU
              regs = executeProgramToStep init prog 100 ^. registers

testSimpleProgram2 = TestCase $ assertEqual
            "ADDI X1 X1 5, BEQ X2 X3 1, ADDI X1 X1 6, ADDI X1 X1 5"
            10
            (readRegister regs X1)
        where prog = M.Program [
                        ADDI (Dest X1) (Source X1) (ImmS 5),
                        BEQ (Source X2) (Source X3) (ImmS 1),
                        ADDI (Dest X1) (Source X1) (ImmS 6),
                        ADDI (Dest X1) (Source X1) (ImmS 5)]
              init = initialCPU
              regs = executeProgramToStep init prog 100 ^. registers

testSimpleProgram3 = TestCase $ assertEqual
            "ADDI X1 X0 5, BEQ X1 X0 3, ADDI X2 X2 5, ADDI X1 X1 (-1), BEQ X0 X0 (-4)"
            25
            (readRegister regs X2)
        where prog = M.Program [
                        ADDI (Dest X1) (Source X0) (ImmS 5),
                        BEQ (Source X1) (Source X0) (ImmS 3),
                        ADDI (Dest X2) (Source X2) (ImmS 5),
                        ADDI (Dest X1) (Source X1) (ImmS (-1)),
                        BEQ (Source X0) (Source X0) (ImmS (-4))]
              init = initialCPU
              regs = executeProgramToStep init prog 100 ^. registers
