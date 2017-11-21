{-# LANGUAGE OverloadedLists #-}
module Memory where

import qualified Data.Vector as V
import Data.Word
import Data.Int
import Data.Maybe

import Instruction
import Registers

newtype Memory = Memory (V.Vector Word32) deriving (Show, Eq)
newtype Program = Program (V.Vector Instruction) deriving (Show, Eq)

emptyMemory :: Memory
emptyMemory = Memory $ V.replicate 256 0

emptyProgram :: Program
emptyProgram = Program []

getMemWord :: Memory -> Word32 -> Word32
getMemWord (Memory mem) addr = fromMaybe (0 :: Word32) $ mem V.!? fromIntegral addr

getMemWords :: Memory -> Int -> Int -> V.Vector Word32
getMemWords (Memory mem) addr len = V.slice addr len mem

setMemWord :: Memory -> Word32 -> Word32 -> Memory
setMemWord (Memory mem) addr val = Memory (mem V.// [(fromIntegral addr, val)])

setMemWords :: Memory -> Int -> [Int32] -> Memory
setMemWords (Memory mem) addr vals = Memory (mem V.// updates)
    where addrs = map (+ addr) [0..(length vals - 1)]
          wordVals = map fromIntegral vals
          updates = zip addrs wordVals

program :: Program
program = Program [
              ADDI (Dest X1) (Source X1) (ImmS 5)
            , ADDI (Dest X2) (Source X1) (ImmS 5)]