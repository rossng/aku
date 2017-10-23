{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module Memory where

import qualified Data.Vector as V
import Data.Word
import Data.Maybe

import Instruction
import Registers

newtype Memory = Memory (V.Vector Word32) deriving (Show, Eq)
newtype Program = Program (V.Vector Instruction) deriving (Show, Eq)

emptyMemory :: Memory
emptyMemory = Memory $ V.replicate 256 0

getMemWord :: Memory -> Word32 -> Word32
getMemWord (Memory mem) addr = fromMaybe (0 :: Word32) $ mem V.!? (fromIntegral addr)

setMemWord :: Memory -> Word32 -> Word32 -> Memory
setMemWord (Memory mem) addr val = Memory (mem V.// [(fromIntegral addr, val)])

program :: Program
program = Program [ADDI (Dest X1) (Source X1) (ImmS 5)]