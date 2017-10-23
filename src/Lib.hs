{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where

import qualified Data.Vector as V
import Data.Word
import Data.Int
import Control.Lens
import Data.Bits
import Data.Maybe

import Registers
import Instruction
import qualified Memory as M

type Stall = Bool
data FuncAlu = AluADD | AluNAND | AluEQ | AluIMM

data BEGIF = BEGIF {
    _begifPc :: Word32
}

makeLenses ''BEGIF

data IFID = IFID {
      _ifidInstruction :: Maybe Instruction
    , _ifidPc :: Word32
}

makeLenses ''IFID

data IDEX = IDEX {
      _idexOp :: Maybe Opcode
    , _idexTarget :: Maybe RegisterName
    , _idexSource1 :: Maybe RegisterName
    , _idexSource2 :: Maybe RegisterName
    , _idexPc :: Word32
    , _idexOperand0 :: Word32
    , _idexOperand1 :: Word32
    , _idexOperand2 :: Word32
}

makeLenses ''IDEX

data EXMEM = EXMEM {
      _exmemOp :: Maybe Opcode
    , _exmemTarget :: Maybe RegisterName
    , _exmemPc :: Word32
    , _exmemStoreData :: Word32
    , _exmemAluOutput :: Word32
}

makeLenses ''EXMEM

data MEMWB = MEMWB {
      _memwbTarget :: Maybe RegisterName
    , _memwbRfWriteData :: Word32
}

makeLenses ''MEMWB

data WBEND = WBEND {
      _wbendTarget :: Maybe RegisterName
    , _wbendRfWriteData :: Word32
}

makeLenses ''WBEND

data CPU = CPU {
      _program :: M.Program
    , _memory :: M.Memory
    , _registers :: Registers
    , _begif :: BEGIF
    , _ifid :: IFID
    , _idex :: IDEX
    , _exmem :: EXMEM
    , _memwb :: MEMWB
    , _wbend :: WBEND
}

makeLenses ''CPU

update :: CPU -> CPU
update cpu = cpu'
    where cpu' = cpu    & begif .~ (begin cpu)
                        & ifid  .~ (fetch cpu)
                        & idex  .~ (decode cpu)
                        & exmem .~ (execute cpu)
                        & memwb .~ (mem cpu)
                        & wbend .~ (writeback cpu)

begin :: CPU -> BEGIF
begin cpu = (cpu^.begif) & begifPc .~ (muxPc cpu)

fetch :: CPU -> IFID
fetch cpu = (cpu^.ifid) & ifidPc .~ begifPc'
                        & ifidInstruction .~ (program' V.!? (fromIntegral $ begifPc'))
            where begifPc' = cpu^.begif.begifPc
                  (M.Program program') = cpu^.program

decode :: CPU -> IDEX
decode cpu = (cpu^.idex)    & idexOp .~ (insToOp <$> (cpu^.ifid.ifidInstruction))
                            & idexTarget .~ ((firstOf (element 0)) =<< writtenRegisters)
                            & idexSource1 .~ source1
                            & idexSource2 .~ source2
                            & idexOperand0 .~ case (getImmediate <$> cpu^.ifid.ifidInstruction) of
                                                (Just imm)  -> imm
                                                Nothing     -> 0
                            & idexOperand1 .~ case readRegister (cpu^.registers) <$> source1 of
                                                (Just val)  -> val
                                                Nothing     -> 0
                            & idexOperand2 .~ case readRegister (cpu^.registers) <$> source2 of
                                                (Just val)  -> val
                                                Nothing     -> 0
    where writtenRegisters = writesRegisters <$> (cpu^.ifid.ifidInstruction)
          source1 = (firstOf (element 0)) =<< (readsRegisters <$> cpu^.ifid.ifidInstruction)
          source2 = (firstOf (element 1)) =<< (readsRegisters <$> cpu^.ifid.ifidInstruction)

execute :: CPU -> EXMEM
execute cpu = (cpu^.exmem)  & exmemOp           .~ (cpu^.idex.idexOp)
                            & exmemTarget       .~ (cpu^.idex.idexTarget)
                            & exmemPc           .~ (cpu^.idex.idexPc)
                            & exmemStoreData    .~ (muxAlu2Reg cpu)
                            & exmemAluOutput    .~ aluOutput
    where (aluOutput, _) = alu cpu

mem :: CPU -> MEMWB
mem cpu = (cpu^.memwb)   & memwbTarget       .~ (cpu^.exmem.exmemTarget)
                         & memwbRfWriteData  .~ (muxMemOut cpu)

writeback :: CPU -> WBEND
writeback cpu = (cpu^.wbend)    & wbendTarget       .~ (cpu^.memwb.memwbTarget)
                                & wbendRfWriteData  .~ (cpu^.memwb.memwbRfWriteData)

updateRegisters :: CPU -> Registers
updateRegisters cpu = case (cpu^.memwb.memwbTarget) of
                        (Just tgt)  -> writeRegister (cpu^.registers) tgt (cpu^.memwb.memwbRfWriteData)
                        Nothing     -> cpu^.registers

updateMemory :: CPU -> M.Memory
updateMemory cpu = case (cpu^.exmem.exmemOp) of
                        (Just OPSW) -> M.setMemWord (cpu^.memory) (cpu^.exmem.exmemAluOutput) (cpu^.exmem.exmemStoreData)
                        Nothing     -> (cpu^.memory)

muxMemOut :: CPU -> Word32
muxMemOut cpu
    | (cpu^.exmem.exmemOp) == (Just OPLW)   = M.getMemWord (cpu^.memory) (cpu^.exmem.exmemAluOutput)
    | otherwise                             = (cpu^.exmem.exmemAluOutput)

-- Forward logic for the source1 register. If the target register of any previous instruction still in the pipeline
-- matches the source1 register, forward it.
muxAlu1 :: CPU -> Word32
muxAlu1 cpu
    | (cpu^.idex.idexSource1) == (cpu^.exmem.exmemTarget)   = cpu^.exmem.exmemAluOutput
    | (cpu^.idex.idexSource1) == (cpu^.memwb.memwbTarget)   = cpu^.memwb.memwbRfWriteData
    | (cpu^.idex.idexSource1) == (cpu^.wbend.wbendTarget)   = cpu^.wbend.wbendRfWriteData
    | otherwise                                             = cpu^.idex.idexOperand1

muxAlu2Reg :: CPU -> Word32
muxAlu2Reg cpu
    | (cpu^.idex.idexSource2) == (cpu^.exmem.exmemTarget)   = cpu^.exmem.exmemAluOutput
    | (cpu^.idex.idexSource2) == (cpu^.memwb.memwbTarget)   = cpu^.memwb.memwbRfWriteData
    | (cpu^.idex.idexSource2) == (cpu^.wbend.wbendTarget)   = cpu^.wbend.wbendRfWriteData
    | otherwise                                             = cpu^.idex.idexOperand2

muxAlu2Imm :: CPU -> Word32
muxAlu2Imm cpu
    | cpu^.idex.idexOp == (Just OPADD)      = muxAlu2Reg cpu
    | cpu^.idex.idexOp == (Just OPADDI)     = cpu^.idex.idexOperand0
    | cpu^.idex.idexOp == (Just OPNAND)     = muxAlu2Reg cpu
    | cpu^.idex.idexOp == (Just OPLUI)      = cpu^.idex.idexOperand0
    | cpu^.idex.idexOp == (Just OPSW)       = cpu^.idex.idexOperand0
    | cpu^.idex.idexOp == (Just OPLW)       = cpu^.idex.idexOperand0
    | cpu^.idex.idexOp == (Just OPBEQ)      = muxAlu2Reg cpu
    | cpu^.idex.idexOp == (Just OPJALR)     = (cpu^.idex.idexPc) + 1

funcAlu :: CPU -> FuncAlu
funcAlu cpu
    | cpu^.idex.idexOp == (Just OPADD)      = AluADD
    | cpu^.idex.idexOp == (Just OPADDI)     = AluADD
    | cpu^.idex.idexOp == (Just OPNAND)     = AluNAND
    | cpu^.idex.idexOp == (Just OPLUI)      = AluIMM
    | cpu^.idex.idexOp == (Just OPSW)       = AluADD
    | cpu^.idex.idexOp == (Just OPLW)       = AluADD
    | cpu^.idex.idexOp == (Just OPBEQ)      = AluEQ
    | cpu^.idex.idexOp == (Just OPJALR)     = AluIMM
    | otherwise                             = AluIMM -- ?

alu :: CPU -> (Word32, Bool)
alu cpu = (output, (muxAlu1 cpu) == (muxAlu2Imm cpu))
    where func = funcAlu cpu
          output = case func of
                    AluADD  -> muxAlu1 cpu + muxAlu2Imm cpu
                    AluNAND -> complement $ muxAlu1 cpu .&. muxAlu2Imm cpu
                    AluEQ   -> 0
                    AluIMM  -> muxAlu2Imm cpu

stomp :: CPU -> Bool
stomp cpu = (eq && (cpu^.idex.idexOp == (Just OPBEQ))) || (cpu^.idex.idexOp == (Just OPJALR))
    where (_, eq) = alu cpu

muxPc :: CPU -> Word32
muxPc cpu
    | eq && (cpu^.idex.idexOp) == (Just OPBEQ)  = (cpu^.idex.idexPc) + 1 + (cpu^.idex.idexOperand0)
    | (cpu^.idex.idexOp) == (Just OPJALR)       = muxAlu1 cpu
    | otherwise                                 = (cpu^.begif.begifPc) + 1
    where (_, eq) = alu cpu

--muxOperand0 :: CPU ->

stall :: CPU -> Bool
stall cpu = ((cpu^.idex.idexOp) == Just OPLW) && clash
    where clash = case do
                    target <- (cpu^.idex.idexTarget)
                    ins <- (cpu^.ifid.ifidInstruction)
                    let reads = readsRegisters ins
                    return (target `elem` reads)
                  of
                    (Just result) -> result
                    Nothing -> False
