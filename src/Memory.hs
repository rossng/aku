{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module Memory where

import qualified Data.Vector as V
import Data.Word

import Instruction
import Registers

newtype Memory = Memory (V.Vector Word8) deriving (Show, Eq)
newtype Program = Program (V.Vector Instruction) deriving (Show, Eq)

emptyMemory :: Memory
emptyMemory = Memory $ V.replicate 256 0

program :: Program
program = Program [ADDI (Dest X1) (Source X1) (ImmS 5)]