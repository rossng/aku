{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BinaryLiterals #-}
import Test.HUnit
import Control.Lens
import Data.Vector as V

import CPU
import Utils
import Instruction
import Registers
import qualified Memory as M

import SingleInstruction
import Forwarding

main :: IO Counts
main = runTestTT tests

tests = TestList [
          TestLabel "Test single instruction" testSingleInstruction
        , TestLabel "Test forwarding" testForwarding
        ]