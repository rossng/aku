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
import Assembler

testSimplePrograms = TestList [
          loadAndTest "programs/simple_loop.asm" testSimpleLoop
        , loadAndTest "programs/infinite_loop.asm" testInfiniteLoop
        , loadAndTest "programs/skip_instruction.asm" testSkipInstruction
    ]

loadAndTest path test = TestCase $ loadProgram path >>= testLoadedProgram test

testLoadedProgram _ Nothing = assertFailure "Failed to load program"
testLoadedProgram test (Just prog) = test prog

testSimpleLoop prog = assertEqual "simple_loop.asm"
             25
             (readRegister regs X2)
         where init = initialCPU
               regs = executeProgramUntilHalt init prog ^. _1 . registers

testInfiniteLoop prog = assertEqual "infinite_loop.asm"
            0
            (readRegister regs X1)
        where init = initialCPU
              regs = executeProgramToStep init prog 100 ^. _1 . registers

testSkipInstruction prog = assertEqual "skip_instruction.asm"
            10
            (readRegister regs X1)
        where init = initialCPU
              regs = executeProgramUntilHalt init prog ^. _1 . registers
