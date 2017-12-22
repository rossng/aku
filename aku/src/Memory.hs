{-# LANGUAGE OverloadedLists #-}
module Memory where

import qualified Data.Vector as V
import Data.Vector.Split
import Data.List
import Data.Word
import Data.Int
import Data.Maybe
import Text.Printf

import Instruction
import Registers

newtype Memory = Memory (V.Vector Word32) deriving (Eq)
newtype Program = Program (V.Vector Instruction) deriving (Show, Eq)

instance Show Memory where
    show (Memory mem) = intercalate "\n" . map (V.foldl (++) "") . chunksOf 16 . V.map (printf "%08x ") $ mem

emptyMemory :: Memory
emptyMemory = Memory $ V.replicate 256 0

emptyProgram :: Program
emptyProgram = Program []

getInstruction :: Program -> Int -> Instruction
getInstruction (Program p) i = p V.! i

getMemWord :: Int -> Memory -> Word32
getMemWord addr (Memory mem) = fromMaybe (0 :: Word32) $ mem V.!? addr

getMemWords :: Int -> Int -> Memory -> V.Vector Word32
getMemWords addr len (Memory mem) = V.slice addr len mem

setMemWord :: Int -> Word32 -> Memory -> Memory
setMemWord addr val (Memory mem) = Memory (mem V.// [(addr, val)])

setMemWords :: Int -> [Int32] -> Memory -> Memory
setMemWords addr vals (Memory mem) = Memory (mem V.// updates)
    where addrs = map (+ addr) [0..(length vals - 1)]
          wordVals = map fromIntegral vals
          updates = zip addrs wordVals

program :: Program
program = Program [
              ADDI (Dest X1) (Source X1) (ImmS 5)
            , ADDI (Dest X2) (Source X1) (ImmS 5)]