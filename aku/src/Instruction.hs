{-# LANGUAGE OverloadedLists #-}
module Instruction where

import Data.Word
import Data.Int

import Registers

newtype DestRegister = Dest RegisterName deriving (Show, Eq)
newtype SourceRegister = Source RegisterName deriving (Show, Eq)
newtype SignedImmediate = ImmS Int32 deriving (Show, Eq)
newtype UnsignedImmediate = ImmU Word32 deriving (Show, Eq)

data Opcode = OPADD | OPADDI | OPNAND | OPSW | OPLW | OPBEQ | OPBLT | OPJALR | OPHALT
              deriving (Eq, Show)

type Instruction = BaseInstruction DestRegister SourceRegister UnsignedImmediate SignedImmediate

data BaseInstruction d s u i =
    -- Add the two source registers and store in the dest
      ADD d s s
    -- Add source register to immediate and store in dest
    | ADDI d s i
    -- Subtract source2 from source1 and store in dest
    | NAND d s s
    --
    | SW s s i
    --
    | LW d s i
    -- Jump to pc + 1 + imm if source1 == source2
    | BEQ s s i
    -- Jump to pc + 1 + imm if source1 < source2
    | BLT s s i
    -- Jump to source and store pc + 1 in dest
    | JALR d s
    | HALT
    deriving (Show, Eq)

nop :: Instruction
nop = ADD (Dest X0) (Source X0) (Source X0)

getImmediate :: Instruction -> Word32
getImmediate ADD{} = 0
getImmediate (ADDI _ _ (ImmS imm)) = fromIntegral imm
getImmediate NAND{} = 0
getImmediate (SW _ _ (ImmS imm)) = fromIntegral imm
getImmediate (LW _ _ (ImmS imm)) = fromIntegral imm
getImmediate (BEQ _ _ (ImmS imm)) = fromIntegral imm
getImmediate (BLT _ _ (ImmS imm)) = fromIntegral imm
getImmediate (JALR _ _) = 0
getImmediate HALT = 0

insToOp :: Instruction -> Opcode
insToOp ADD{} = OPADD
insToOp ADDI{} = OPADDI
insToOp NAND{} = OPNAND
insToOp SW{} = OPSW
insToOp LW{} = OPLW
insToOp BEQ{} = OPBEQ
insToOp BLT{} = OPBLT
insToOp JALR{} = OPJALR
insToOp HALT = OPHALT

-- What registers does an instruction write (except PC)?
writesRegister :: Instruction -> Maybe RegisterName
writesRegister (ADD (Dest r) _ _) = Just r
writesRegister (ADDI (Dest r) _ _) = Just r
writesRegister (NAND (Dest r) _ _) = Just r
writesRegister SW{} = Nothing
writesRegister (LW (Dest r) _ _) = Just r
writesRegister BEQ{} = Nothing
writesRegister BLT{} = Nothing
writesRegister (JALR (Dest r) _) = Just r
writesRegister HALT = Nothing

-- What registers does an instruction read (except PC)?
readsRegisters :: Instruction -> [RegisterName]
readsRegisters (ADD _ (Source r1) (Source r2)) = [r1, r2]
readsRegisters (ADDI _ (Source r1) _) = [r1]
readsRegisters (NAND _ (Source r1) (Source r2)) = [r1, r2]
readsRegisters (SW (Source r1) (Source r2) _) = [r1, r2]
readsRegisters (LW _ (Source r1) _) = [r1]
readsRegisters (BEQ (Source r1) (Source r2) _) = [r1, r2]
readsRegisters (BLT (Source r1) (Source r2) _) = [r1, r2]
readsRegisters (JALR _ (Source r1)) = [r1]
readsRegisters HALT = []

source1 :: Instruction -> Maybe RegisterName
source1 (ADD _ (Source r) _) = Just r
source1 (ADDI _ (Source r) _) = Just r
source1 (NAND _ (Source r) _) = Just r
source1 (SW (Source r) _ _) = Just r
source1 (LW _ (Source r) _) = Just r
source1 (BEQ (Source r) _ _) = Just r
source1 (BLT (Source r) _ _) = Just r
source1 (JALR _ (Source r)) = Just r
source1 HALT = Nothing

source2 :: Instruction -> Maybe RegisterName
source2 (ADD _ _ (Source r)) = Just r
source2 ADDI{} = Nothing
source2 (NAND _ _ (Source r)) = Just r
source2 (SW _ (Source r) _) = Just r
source2 LW{} = Nothing
source2 (BEQ _ (Source r) _) = Just r
source2 (BLT _ (Source r) _) = Just r
source2 JALR{} = Nothing
source2 HALT = Nothing