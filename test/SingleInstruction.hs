{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BinaryLiterals #-}
module SingleInstruction where

import Test.HUnit
import Control.Lens
import Data.Vector as V

import CPU
import Utils
import Instruction
import Registers
import qualified Memory as M

testSingleInstruction = TestList [
      testADD
    , testADDI
    , testNAND
    , testSW
    , testLW
    , testBEQ
    , testJALR
    ]

testADD = TestCase $ assertEqual
            "ADD X1 X2 X3"
            10
            (readRegister regs X1)
        where prog = M.Program [ADD (Dest X1) (Source X2) (Source X3)]
              init = initialCPU & (registers.x2) .~ 7
                                & (registers.x3) .~ 3
              regs = executeProgramToStep init prog 6 ^. _1 . registers

testADDI = TestCase $ assertEqual
            "ADDI X1 X1 5"
            5
            (readRegister regs X1)
           where prog = M.Program [ADDI (Dest X1) (Source X1) (ImmS 5)]
                 regs = executeProgramToStep initialCPU prog 6 ^. _1 . registers

testNAND = TestCase $ assertEqual
            "NAND X1 X2 X3"
            0b01111101111111111111101111111101
            (readRegister regs X1)
           where prog = M.Program [NAND (Dest X1) (Source X2) (Source X3)]
                 init = initialCPU & (registers.x2) .~ 0b10000010000000000000010000000010
                                   & (registers.x3) .~ 0b10000010000000000000010000001110
                 regs = executeProgramToStep init prog 6 ^. _1 . registers

testSW = TestCase $ assertEqual
            "SW X1 X2 2"
            15
            (M.getMemWord mem 2)
           where prog = M.Program [SW (Source X1) (Source X2) (ImmS (-1))]
                 init = initialCPU & (registers.x2) .~ 3
                                   & (registers.x1) .~ 15
                 mem = executeProgramToStep init prog 6 ^. _1 . memory

testLW = TestCase $ assertEqual
            "LW X1 X2 2"
            25
            (readRegister regs X1)
           where prog = M.Program [LW (Dest X1) (Source X2) (ImmS 5)]
                 init = initialCPU & (registers.x2) .~ 4
                                   & memory .~ M.setMemWord (initialCPU^.memory) 9 25
                 regs = executeProgramToStep init prog 6 ^. _1 . registers

testBEQ = TestCase $ assertEqual
            "BEQ X1 X2 2"
            7
            (result^.begif.begifPc)
           where prog = M.Program [BEQ (Source X1) (Source X2) (ImmS 5)]
                 init = initialCPU & (registers.x1) .~ 4
                                   & (registers.x2) .~ 4
                 (result, _) = executeProgramToStep init prog 4

testJALR = TestList [
            TestCase $ assertEqual
                "JALR X1 X2: PC"
                13
                (result^.begif.begifPc),
            TestCase $ assertEqual
                "JALR X1 X2: X1"
                1
                (readRegister (result^.registers) X1)
           ]
           where prog = M.Program [JALR (Dest X1) (Source X2)]
                 init = initialCPU & (registers.x2) .~ 10
                 (result, _) = executeProgramToStep init prog 6