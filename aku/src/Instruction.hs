{-# LANGUAGE OverloadedLists #-}
module Instruction where

import Data.Word
import Data.Int

import Registers

newtype DestRegister = Dest RegisterName deriving (Show, Eq)
newtype SourceRegister = Source RegisterName deriving (Show, Eq)
newtype SignedImmediate = ImmS Int32 deriving (Show, Eq)
newtype UnsignedImmediate = ImmU Word32 deriving (Show, Eq)

data Opcode = OPADD | OPADDI | OPNAND | OPLUI | OPSW | OPLW | OPBEQ | OPJALR | OPHALT
              deriving (Eq, Show)

data Instruction =
    -- Add the two source registers and store in the dest
      ADD DestRegister SourceRegister SourceRegister
    -- Add source register to immediate and store in dest
    | ADDI DestRegister SourceRegister SignedImmediate
    -- Subtract source2 from source1 and store in dest
    | NAND DestRegister SourceRegister SourceRegister
    -- Subtract immediate from source and store in dest
    | LUI DestRegister UnsignedImmediate
    --
    | SW SourceRegister SourceRegister SignedImmediate
    --
    | LW DestRegister SourceRegister SignedImmediate
    -- Jump to pc + 1 + imm if source1 == source2
    | BEQ SourceRegister SourceRegister SignedImmediate
    -- Jump to source and store pc + 1 in dest
    | JALR DestRegister SourceRegister
    | HALT
    deriving (Show, Eq)

nop :: Instruction
nop = ADD (Dest X0) (Source X0) (Source X0)

getImmediate :: Instruction -> Word32
getImmediate ADD{} = 0
getImmediate (ADDI _ _ (ImmS imm)) = fromIntegral imm
getImmediate NAND{} = 0
getImmediate (LUI _ (ImmU imm)) = fromIntegral imm
getImmediate (SW _ _ (ImmS imm)) = fromIntegral imm
getImmediate (LW _ _ (ImmS imm)) = fromIntegral imm
getImmediate (BEQ _ _ (ImmS imm)) = fromIntegral imm
getImmediate (JALR _ _) = 0
getImmediate HALT = 0

insToOp :: Instruction -> Opcode
insToOp ADD{} = OPADD
insToOp ADDI{} = OPADDI
insToOp NAND{} = OPNAND
insToOp LUI{} = OPLUI
insToOp SW{} = OPSW
insToOp LW{} = OPLW
insToOp BEQ{} = OPBEQ
insToOp JALR{} = OPJALR
insToOp HALT = OPHALT

-- What registers does an instruction write (except PC)?
writesRegisters :: Instruction -> [RegisterName]
writesRegisters (ADD (Dest r) _ _) = [r]
writesRegisters (ADDI (Dest r) _ _) = [r]
writesRegisters (NAND (Dest r) _ _) = [r]
writesRegisters (LUI (Dest r) _) = [r]
writesRegisters SW{} = []
writesRegisters (LW (Dest r) _ _) = [r]
writesRegisters BEQ{} = []
writesRegisters (JALR (Dest r) _) = [r]
writesRegisters HALT = []

-- What registers does an instruction read (except PC)?
readsRegisters :: Instruction -> [RegisterName]
readsRegisters (ADD _ (Source r1) (Source r2)) = [r1, r2]
readsRegisters (ADDI _ (Source r1) _) = [r1]
readsRegisters (NAND _ (Source r1) (Source r2)) = [r1, r2]
readsRegisters (LUI _ _) = []
readsRegisters (SW (Source r1) (Source r2) _) = [r1, r2]
readsRegisters (LW _ (Source r1) _) = [r1]
readsRegisters (BEQ (Source r1) (Source r2) _) = [r1, r2]
readsRegisters (JALR _ (Source r1)) = [r1]
readsRegisters HALT = []


operand1 :: Instruction -> Maybe RegisterName
operand1 (ADD _ (Source r) _) = Just r
operand1 (ADDI _ (Source r) _) = Just r
operand1 (NAND _ (Source r) _) = Just r
operand1 LUI{} = Nothing
operand1 (SW _ (Source r) _) = Just r
operand1 (LW _ (Source r) _) = Just r
operand1 (BEQ _ (Source r) _) = Just r
operand1 (JALR _ (Source r)) = Just r
operand1 HALT = Nothing

operand2 :: Instruction -> Maybe RegisterName
operand2 (ADD _ _ (Source r)) = Just r
operand2 ADDI{} = Nothing
operand2 (NAND _ _ (Source r)) = Just r
operand2 LUI{} = Nothing
operand2 (SW (Source r) _ _) = Just r
operand2 LW{} = Nothing
operand2 (BEQ (Source r) _ _) = Just r
operand2 JALR{} = Nothing
operand2 HALT = Nothing