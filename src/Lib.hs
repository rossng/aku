{-# LANGUAGE OverloadedLists #-}
module Lib where

import qualified Data.Vector as V
import Data.Word
import Data.Int

import Registers

newtype DestRegister = Dest Register deriving (Show, Eq)
newtype SourceRegister = Source Register deriving (Show, Eq)
newtype SignedImmediate = ImmS Int32 deriving (Show, Eq)
newtype UnsignedImmediate = ImmU Word32 deriving (Show, Eq)

data Instruction =
    -- Add the two source registers and store in the dest
      ADD SourceRegister SourceRegister DestRegister
    -- Add source register to immediate and store in dest
    | ADDI SourceRegister SignedImmediate DestRegister
    -- Subtract source2 from source1 and store in dest
    | SUB SourceRegister SourceRegister DestRegister
    -- Subtract immediate from source and store in dest
    | SUBI SourceRegister SignedImmediate DestRegister
    -- Load byte from address source + imm into dest
--    | LB SourceRegister SignedImmediate DestRegister
    -- Store byte from source2 to address source1 + imm
--    | SB SourceRegister SignedImmediate SourceRegister
    -- Jump to pc + imm
    | J SignedImmediate
    -- Jump to pc + imm if source1 == source2
    | BEQ SourceRegister SourceRegister SignedImmediate
    -- Jump to pc + imm if source1 < source2
    | BLT SourceRegister SourceRegister SignedImmediate
    deriving (Show, Eq)

data DecodedInstruction =
      DecodedADD Int32 Int32 DestRegister
    | DecodedADDI Int32 Int32 DestRegister
    | DecodedSUB Int32 Int32 DestRegister
    | DecodedSUBI Int32 Int32 DestRegister
--    | DecodedLB Int32 Int32 DestRegister
--    | DecodedSB Int32 Int32 Int32
    | DecodedJ Int32
    | DecodedBEQ Int32 Int32 Int32
    | DecodedBLT Int32 Int32 Int32
    deriving (Show, Eq)

newtype Memory = Memory (V.Vector Word8) deriving (Show, Eq)
newtype Program = Program (V.Vector Instruction) deriving (Show, Eq)
newtype PC = PC Int deriving (Show, Eq)

newtype Decode = Decode (Maybe Instruction) deriving (Show, Eq)
newtype Execute = Execute (Maybe DecodedInstruction) deriving (Show, Eq)

data CPU = CPU Registers PC Program Decode Execute deriving (Show, Eq)

defaultCPU :: CPU
defaultCPU = CPU emptyRegisters (PC 0) program (Decode Nothing) (Execute Nothing)

cycle :: CPU -> CPU
cycle (CPU registers pc program decode execute) =
    (CPU registers' pc' program decode' execute')
    where (decode', pc') = fetchC pc program
          execute' = decodeC registers decode
          registers' = executeC registers execute

fetchC :: PC -> Program -> (Decode, PC)
fetchC (PC pc) (Program program) = (Decode (program V.!? pc), PC (pc + 1))

decodeC :: Registers -> Decode -> Execute
decodeC registers (Decode (Just instruction)) = Execute $ case instruction of
    ADD (Source source1) (Source source2) dest -> Just $ DecodedADD
        (fromIntegral $ readRegister registers source1)
        (fromIntegral $ readRegister registers source2)
        dest
    ADDI (Source source1) (ImmS imm) dest -> Just $ DecodedADDI
        (fromIntegral $ readRegister registers source1)
        imm
        dest
    _ -> Nothing
decodeC registers (Decode Nothing) = Execute Nothing

executeC :: Registers -> Execute -> Registers
executeC registers (Execute (Just instruction)) = case instruction of
    DecodedADD op1 op2 (Dest dest) -> writeRegister registers dest (fromIntegral $ op1 + op2)
    DecodedADDI op1 op2 (Dest dest) -> writeRegister registers dest (fromIntegral $ op1 + op2)
    _ -> registers
executeC registers (Execute Nothing) = registers

emptyMemory :: Memory
emptyMemory = Memory $ V.replicate 256 0

program :: Program
program = Program [ADDI (Source X1) (ImmS 5) (Dest X1)]
