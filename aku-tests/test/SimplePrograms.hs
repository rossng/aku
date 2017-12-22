{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BinaryLiterals #-}
module SimplePrograms where

import Test.HUnit
import Control.Lens
import Data.Vector as V
import Data.List as L
import Data.Int

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
        , loadAndTest "programs/bubble_sort.asm" (testBubbleSort [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15])
        , loadAndTest "programs/bubble_sort.asm" (testBubbleSort [2,12,1,4,9,6,14,7,10,5,11,13,15,0,8,3])
        , loadAndTest "programs/bubble_sort.asm" (testBubbleSort [3,2])
        , loadAndTest "programs/bubble_sort.asm" (testBubbleSort [70,60,50,40,30,20,10,0,-10,-20,-30,-40,-50,-60,-70,-80])
        , loadAndTest "programs/bubble_sort.asm" (testBubbleSort [-1])
        , loadAndTest "programs/bubble_sort.asm" (testBubbleSort [1])
        , loadAndTest "programs/bubble_sort.asm" (testBubbleSort [1,2])
        , loadAndTest "programs/bubble_sort.asm" (testBubbleSort [5,4,3,2,1,-1,-2,-3,-4])
        , loadAndTest "programs/bubble_sort.asm" (testBubbleSort [-1,-1,-1])
        , loadAndTest "programs/store_then_load.asm" testStoreThenLoad
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

testBubbleSort :: [Int32] -> M.Program -> Assertion
testBubbleSort list prog = assertEqual "bubble_sort.asm"
            (fromList (sort list))
            (V.map fromIntegral $ M.getMemWords 0 (L.length list) mem)
        where init = initialCPU & memory .~ M.setMemWords 0 list M.emptyMemory
                                & (registers.x1) .~ fromIntegral (L.length list)
              mem = executeProgramUntilHalt init prog ^. _1 . memory

testStoreThenLoad prog = assertEqual "store_then_load.asm"
            10
            (readRegister regs X4)
        where init = initialCPU
              regs = executeProgramUntilHalt init prog ^. _1 . registers