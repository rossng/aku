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
    , testADDNeg
    , testADDI
    , testMUL
    , testMULNeg
    , testNAND
    , testSW
    , testLW
    , testBEQTaken
    , testBEQSkipped
    , testBLTTaken
    , testBLTSkipped
    , testJALR
    ]

testADD = TestCase $ assertEqual
            "ADD X1 X2 X3"
            10
            (readRegister regs X1)
        where prog = M.Program [ADD (Dest X1) (Source X2) (Source X3), HALT]
              init = initialCPU & (registers.x2) .~ 7
                                & (registers.x3) .~ 3
              regs = executeProgramUntilHalt init prog ^. _1 . registers

testADDNeg = TestCase $ assertEqual
            "ADD X1 X2 X3"
            4
            (readRegister regs X1)
        where prog = M.Program [ADD (Dest X1) (Source X2) (Source X3), HALT]
              init = initialCPU & (registers.x2) .~ 7
                                & (registers.x3) .~ (-3)
              regs = executeProgramUntilHalt init prog ^. _1 . registers

testADDI = TestCase $ assertEqual
            "ADDI X1 X1 5"
            5
            (readRegister regs X1)
           where prog = M.Program [ADDI (Dest X1) (Source X1) (ImmS 5), HALT]
                 regs = executeProgramUntilHalt initialCPU prog ^. _1 . registers

testMUL = TestCase $ assertEqual
            "MUL X1 X2 X3"
            30
            (readRegister regs X1)
        where prog = M.Program [ADD (Dest X1) (Source X2) (Source X3), HALT]
              init = initialCPU & (registers.x2) .~ 5
                                & (registers.x3) .~ 6
              regs = executeProgramUntilHalt init prog ^. _1 . registers

testMULNeg = TestCase $ assertEqual
            "MUL X1 X2 X3"
            (-8)
            (readRegister regs X1)
        where prog = M.Program [ADD (Dest X1) (Source X2) (Source X3), HALT]
              init = initialCPU & (registers.x2) .~ (-2)
                                & (registers.x3) .~ 4
              regs = executeProgramUntilHalt init prog ^. _1 . registers

testNAND = TestCase $ assertEqual
            "NAND X1 X2 X3"
            0b01111101111111111111101111111101
            (readRegister regs X1)
           where prog = M.Program [NAND (Dest X1) (Source X2) (Source X3), HALT]
                 init = initialCPU & (registers.x2) .~ 0b10000010000000000000010000000010
                                   & (registers.x3) .~ 0b10000010000000000000010000001110
                 regs = executeProgramUntilHalt init prog ^. _1 . registers

testSW = TestCase $ assertEqual
            "SW X1 X2 2"
            15
            (M.getMemWord 2 mem)
           where prog = M.Program [SW (Source X1) (Source X2) (ImmS (-1)), HALT]
                 init = initialCPU & (registers.x2) .~ 3
                                   & (registers.x1) .~ 15
                 mem = executeProgramUntilHalt init prog ^. _1 . memory

testLW = TestCase $ assertEqual
            "LW X1 X2 2"
            25
            (readRegister regs X1)
           where prog = M.Program [LW (Dest X1) (Source X2) (ImmS 5), HALT]
                 init = initialCPU & (registers.x2) .~ 4
                                   & memory %~ M.setMemWord 9 25
                 regs = executeProgramUntilHalt init prog ^. _1 . registers

testBEQTaken = TestCase $ assertEqual
            "BEQ X0 X1 1, ADDI X2 X0 5, HALT"
            0
            (readRegister regs X2)
           where prog = M.Program [ BEQ (Source X0) (Source X1) (ImmS 1)
                                  , ADDI (Dest X2) (Source X0) (ImmS 5)
                                  , HALT]
                 regs = executeProgramUntilHalt initialCPU prog ^. _1 . registers

testBEQSkipped = TestCase $ assertEqual
            "ADDI X1 X0 1, BEQ X0 X1 1, ADDI X2 X0 5, HALT"
            5
            (readRegister regs X2)
           where prog = M.Program [ ADDI (Dest X1) (Source X0) (ImmS 1)
                                  , BEQ (Source X0) (Source X1) (ImmS 1)
                                  , ADDI (Dest X2) (Source X0) (ImmS 5)
                                  , HALT]
                 regs = executeProgramUntilHalt initialCPU prog ^. _1 . registers

testBLTTaken = TestCase $ assertEqual
            "ADDI X1 X0 1, BLT X0 X1 1, ADDI X2 X0 5, HALT"
            0
            (readRegister regs X2)
            where prog = M.Program [ ADDI (Dest X1) (Source X0) (ImmS 1)
                                   , BLT (Source X0) (Source X1) (ImmS 1)
                                   , ADDI (Dest X2) (Source X0) (ImmS 5)
                                   , HALT]
                  regs = executeProgramUntilHalt initialCPU prog ^. _1 . registers

testBLTSkipped = TestCase $ assertEqual
            "ADDI X1 X0 1, BLT X1 X0 1, ADDI X2 X0 5, HALT"
            5
            (readRegister regs X2)
            where prog = M.Program [ ADDI (Dest X1) (Source X0) (ImmS 1)
                                   , BLT (Source X1) (Source X0) (ImmS 1)
                                   , ADDI (Dest X2) (Source X0) (ImmS 5)
                                   , HALT]
                  regs = executeProgramUntilHalt initialCPU prog ^. _1 . registers

testJALR = TestList [
            TestCase $ assertEqual
                "JALR X1 X2 (2), ADDI X3 X0 1, HALT"
                0
                (readRegister (result^.registers) X3)
           ]
           where prog = M.Program [JALR (Dest X1) (Source X2), ADDI (Dest X3) (Source X0) (ImmS 1), HALT]
                 init = initialCPU & (registers.x2) .~ 2
                 (result, _) = executeProgramToStep init prog 6