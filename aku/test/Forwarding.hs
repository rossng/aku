{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BinaryLiterals #-}
module Forwarding where

import Test.HUnit
import Control.Lens
import Data.Vector as V

import CPU
import Utils
import Instruction
import Registers
import qualified Memory as M

testForwarding = TestList [
        testForwarding1
      , testForwarding2
--       , testForwarding3
--       , testForwarding4
--       , testForwarding5
--       , testForwarding6
    ]

testForwarding1 = TestCase $ assertEqual
            "ADD X1 X2 X3, ADDI X1 X1 5"
            15
            (readRegister regs X1)
        where prog = M.Program [
                        ADD (Dest X1) (Source X2) (Source X3),
                        ADDI (Dest X1) (Source X1) (ImmS 5),
                        HALT]
              init = initialCPU & (registers.x2) .~ 7
                                & (registers.x3) .~ 3
              regs = executeProgramUntilHalt init prog ^. _1 . registers

testForwarding2 = TestCase $ assertEqual
            "ADD X1 X2 X3, ADD X0 X0 X0, ADDI X1 X1 5"
            15
            (readRegister regs X1)
        where prog = M.Program [
                        ADD (Dest X1) (Source X2) (Source X3),
                        ADD (Dest X0) (Source X0) (Source X0),
                        ADDI (Dest X1) (Source X1) (ImmS 5),
                        HALT]
              init = initialCPU & (registers.x2) .~ 7
                                & (registers.x3) .~ 3
              regs = executeProgramUntilHalt init prog ^. _1 . registers

testForwarding3 = TestCase $ assertEqual
            "ADD X1 X2 X3, ADD X0 X0 X0, ADD X0 X0 X0, ADDI X1 X1 5"
            15
            (readRegister regs X1)
        where prog = M.Program [
                        ADD (Dest X1) (Source X2) (Source X3),
                        ADD (Dest X0) (Source X0) (Source X0),
                        ADD (Dest X0) (Source X0) (Source X0),
                        ADDI (Dest X1) (Source X1) (ImmS 5),
                        HALT]
              init = initialCPU & (registers.x2) .~ 7
                                & (registers.x3) .~ 3
              regs = executeProgramUntilHalt init prog ^. _1 . registers

testForwarding4 = TestCase $ assertEqual
            "ADD X1 X2 X3, ADD X0 X0 X0, ADD X0 X0 X0, ADD X0 X0 X0, ADDI X1 X1 5"
            15
            (readRegister regs X1)
        where prog = M.Program [
                        ADD (Dest X1) (Source X2) (Source X3),
                        ADD (Dest X0) (Source X0) (Source X0),
                        ADD (Dest X0) (Source X0) (Source X0),
                        ADD (Dest X0) (Source X0) (Source X0),
                        ADDI (Dest X1) (Source X1) (ImmS 5),
                        HALT]
              init = initialCPU & (registers.x2) .~ 7
                                & (registers.x3) .~ 3
              regs = executeProgramUntilHalt init prog ^. _1 . registers

testForwarding5 = TestCase $ assertEqual
            "ADD X1 X2 X3, ADD X0 X0 X0, ADD X0 X0 X0, ADD X0 X0 X0, ADD X0 X0 X0, ADDI X1 X1 5"
            15
            (readRegister regs X1)
        where prog = M.Program [
                        ADD (Dest X1) (Source X2) (Source X3),
                        ADD (Dest X0) (Source X0) (Source X0),
                        ADD (Dest X0) (Source X0) (Source X0),
                        ADD (Dest X0) (Source X0) (Source X0),
                        ADD (Dest X0) (Source X0) (Source X0),
                        ADDI (Dest X1) (Source X1) (ImmS 5),
                        HALT]
              init = initialCPU & (registers.x2) .~ 7
                                & (registers.x3) .~ 3
              regs = executeProgramUntilHalt init prog ^. _1 . registers

testForwarding6 = TestCase $ assertEqual
            "ADD X4 X2 X3, ADD X0 X0 X0, ADDI X4 X4 5, ADD X0 X0 X0, ADDI X4 X4 5"
            20
            (readRegister regs X4)
        where prog = M.Program [
                        ADD (Dest X4) (Source X2) (Source X3),
                        ADD (Dest X0) (Source X0) (Source X0),
                        ADDI (Dest X4) (Source X4) (ImmS 5),
                        ADD (Dest X0) (Source X0) (Source X0),
                        ADDI (Dest X4) (Source X4) (ImmS 5),
                        HALT]
              init = initialCPU & (registers.x2) .~ 7
                                & (registers.x3) .~ 3
              regs = executeProgramUntilHalt init prog ^. _1 . registers
