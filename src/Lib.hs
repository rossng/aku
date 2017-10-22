{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where

import qualified Data.Vector as V
import Data.Word
import Data.Int
import Control.Lens

import Registers
import Instruction
import qualified Memory as M

type Stall = Bool

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
    where cpu' = cpu & ifid .~ (fetch cpu)

fetch :: CPU -> IFID
fetch cpu = (cpu^.ifid) & ifidPc .~ begifPc'
                        & ifidInstruction .~ (program' V.!? (fromIntegral $ begifPc'))
            where begifPc' = cpu^.begif.begifPc
                  (M.Program program') = cpu^.program

decode :: CPU -> IDEX
decode cpu = (cpu^.idex) & idexOp .~ exOp'
                         & idexTarget .~ exTarget'
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
    where (exOp', exTarget', exSource2', stall) = ctl7 (cpu^.ifid.ifidInstruction) (cpu^.idex.idexOp) (cpu^.idex.idexTarget)
          source1 = (firstOf (element 0)) =<< (readsRegisters <$> cpu^.ifid.ifidInstruction)
          source2 = (firstOf (element 1)) =<< (readsRegisters <$> cpu^.ifid.ifidInstruction)

ctl7 :: Maybe Instruction -> Maybe Opcode -> Maybe RegisterName -> (Maybe Opcode, Maybe RegisterName, Maybe RegisterName, Stall)
ctl7 idIns exOp exTarget = (exOp', exTarget', exSource2', stall)
    where stall = (exOp == Just OPLW) && clash
          clash = case do
                    target <- exTarget
                    ins <- idIns
                    let reads = readsRegisters ins
                    return (target `elem` reads)
                  of
                    (Just result) -> result
                    Nothing -> False
          exOp' = insToOp <$> idIns
          exTarget' = (firstOf (element 0)) =<< (writesRegisters <$> idIns)
          exSource2' = (firstOf (element 1)) =<< (readsRegisters <$> idIns)
