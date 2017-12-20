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

sourceReg1 :: Instruction -> Maybe RegisterName
sourceReg1 insn = case source1 insn of
    Just (Source s) -> Just s
    Nothing         -> Nothing

sourceReg2 :: Instruction -> Maybe RegisterName
sourceReg2 insn = case source2 insn of
    Just (Source s) -> Just s
    Nothing         -> Nothing

source1 :: BaseInstruction d s u i -> Maybe s
source1 (ADD _ s _) = Just s
source1 (ADDI _ s _) = Just s
source1 (NAND _ s _) = Just s
source1 (SW s _ _) = Just s
source1 (LW _ s _) = Just s
source1 (BEQ s _ _) = Just s
source1 (BLT s _ _) = Just s
source1 (JALR _ s) = Just s
source1 HALT = Nothing

source2 :: BaseInstruction d s u i -> Maybe s
source2 (ADD _ _ s) = Just s
source2 ADDI{} = Nothing
source2 (NAND _ _ s) = Just s
source2 (SW _ s _) = Just s
source2 LW{} = Nothing
source2 (BEQ _ s _) = Just s
source2 (BLT _ s _) = Just s
source2 JALR{} = Nothing
source2 HALT = Nothing

updateSource1 :: s -> BaseInstruction d s u i -> BaseInstruction d s u i
updateSource1 s' (ADD d s1 s2) = ADD d s' s2
updateSource1 s' (ADDI d s1 i) = ADDI d s' i
updateSource1 s' (NAND d s1 s2) = NAND d s' s2
updateSource1 s' (SW s1 s2 i) = SW s' s2 i
updateSource1 s' (LW d s1 i) = LW d s' i
updateSource1 s' (BEQ s1 s2 i) = BEQ s' s2 i
updateSource1 s' (BLT s1 s2 i) = BLT s' s2 i
updateSource1 s' (JALR d s) = JALR d s'
updateSource1 s' HALT = HALT

updateSource2 :: s -> BaseInstruction d s u i -> BaseInstruction d s u i
updateSource2 s' (ADD d s1 s2) = ADD d s1 s'
updateSource2 s' (ADDI d s1 i) = ADDI d s1 i
updateSource2 s' (NAND d s1 s2) = NAND d s1 s'
updateSource2 s' (SW s1 s2 i) = SW s1 s' i
updateSource2 s' (LW d s1 i) = LW d s1 i
updateSource2 s' (BEQ s1 s2 i) = BEQ s1 s' i
updateSource2 s' (BLT s1 s2 i) = BLT s1 s' i
updateSource2 s' (JALR d s) = JALR d s
updateSource2 s' HALT = HALT