{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module CPU where

import qualified Data.Vector as V
import Data.Word
import Data.Int
import Control.Lens
import Data.Bits
import Data.Maybe
import Control.Monad.Writer

import Registers
import Instruction
import qualified Memory as M

type Stall = Bool
data FuncAlu = AluADD | AluNAND | AluEQ | AluIMM deriving (Eq, Show)

data BEGIF = BEGIF {
    _begifPc :: Word32
} deriving (Eq, Show)

initialBEGIF :: BEGIF
initialBEGIF = BEGIF 0

makeLenses ''BEGIF

data IFID = IFID {
      _ifidInstruction :: Maybe Instruction
    , _ifidPc :: Word32
} deriving (Eq, Show)

initialIFID :: IFID
initialIFID = IFID Nothing 0

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
} deriving (Eq, Show)

initialIDEX :: IDEX
initialIDEX = IDEX Nothing Nothing Nothing Nothing 0 0 0 0

makeLenses ''IDEX

data EXMEM = EXMEM {
      _exmemOp :: Maybe Opcode
    , _exmemTarget :: Maybe RegisterName
    , _exmemPc :: Word32
    , _exmemStoreData :: Word32
    , _exmemAluOutput :: Word32
} deriving (Eq, Show)

initialEXMEM :: EXMEM
initialEXMEM = EXMEM Nothing Nothing 0 0 0

makeLenses ''EXMEM

data MEMWB = MEMWB {
      _memwbTarget :: Maybe RegisterName
    , _memwbRfWriteData :: Word32
} deriving (Eq, Show)

initialMEMWB :: MEMWB
initialMEMWB = MEMWB Nothing 0

makeLenses ''MEMWB

data WBEND = WBEND {
      _wbendTarget :: Maybe RegisterName
    , _wbendRfWriteData :: Word32
} deriving (Eq, Show)

initialWBEND :: WBEND
initialWBEND = WBEND Nothing 0

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
} deriving (Eq, Show)

makeLenses ''CPU

data Stats = Stats {
      _statsCycles :: Int
} deriving (Eq, Show)

makeLenses ''Stats

instance Monoid Stats where
    mempty = Stats 0
    Stats a `mappend` Stats b = Stats (a + b)

initialCPU :: CPU
initialCPU = CPU M.emptyProgram M.emptyMemory emptyRegisters initialBEGIF initialIFID initialIDEX initialEXMEM initialMEMWB initialWBEND

update ::CPU -> Writer Stats CPU
update cpu = writer (cpu', Stats 1)
  where
    cpu' = cpu  & begif .~ begif'
                & ifid  .~ ifid''
                & idex  .~ idex'
                & exmem .~ execute cpu
                & memwb .~ mem cpu
                & wbend .~ writeback cpu
                & registers .~ writeRegisters cpu
                & memory .~ writeMemory cpu
    begif' = if stall cpu then cpu^.begif else begin cpu
    ifid' = if stall cpu then cpu^.ifid else fetch cpu
    ifid'' = if stomp cpu then ifid' & ifidInstruction .~ Just nop else ifid'
    idex' = if stomp cpu then nopIdex (cpu^.idex) else decode cpu

writeMemory :: CPU -> M.Memory
writeMemory cpu = if (cpu^.exmem.exmemOp) == Just OPSW
                    then M.setMemWord (cpu^.memory) (cpu^.exmem.exmemAluOutput) (cpu^.exmem.exmemStoreData)
                    else cpu^.memory

writeRegisters :: CPU -> Registers
writeRegisters cpu = case cpu^.memwb.memwbTarget of
                        Nothing     -> cpu^.registers
                        (Just reg)  -> writeRegister (cpu^.registers) reg (cpu^.memwb.memwbRfWriteData)

nopIdex :: IDEX -> IDEX
nopIdex i = i   & idexOp        .~ Just OPADD
                & idexTarget    .~ Just X0
                & idexSource1   .~ Just X0
                & idexSource2   .~ Just X0
                & idexOperand0  .~ 0
                & idexOperand1  .~ 0
                & idexOperand2  .~ 0

begin :: CPU -> BEGIF
begin cpu = (cpu^.begif) & begifPc .~ muxPc cpu

fetch :: CPU -> IFID
fetch cpu = (cpu^.ifid) & ifidPc .~ begifPc'
                        & ifidInstruction .~ (program' V.!? fromIntegral begifPc')
            where begifPc' = cpu^.begif.begifPc
                  (M.Program program') = cpu^.program

decode :: CPU -> IDEX
decode cpu = (cpu^.idex)    & idexOp .~ (insToOp <$> (cpu^.ifid.ifidInstruction))
                            & idexTarget .~ (firstOf (element 0) =<< writtenRegisters)
                            & idexSource1 .~ operand1Register
                            & idexSource2 .~ operand2Register
                            & idexPc .~ (cpu^.ifid.ifidPc)
                            & idexOperand0 .~ case getImmediate <$> cpu^.ifid.ifidInstruction of
                                                (Just imm)  -> imm
                                                Nothing     -> 0
                            & idexOperand1 .~ case readRegister (cpu^.registers) <$> operand1Register of
                                                (Just val)  -> val
                                                Nothing     -> 0
                            & idexOperand2 .~ case readRegister (cpu^.registers) <$> operand2Register of
                                                (Just val)  -> val
                                                Nothing     -> 0
    where writtenRegisters = writesRegisters <$> (cpu^.ifid.ifidInstruction)
          operand1Register = cpu^.ifid.ifidInstruction >>= operand1
          operand2Register = cpu^.ifid.ifidInstruction >>= operand2

execute :: CPU -> EXMEM
execute cpu = (cpu^.exmem)  & exmemOp           .~ cpu^.idex.idexOp
                            & exmemTarget       .~ cpu^.idex.idexTarget
                            & exmemPc           .~ cpu^.idex.idexPc
                            & exmemStoreData    .~ muxAlu2Reg cpu
                            & exmemAluOutput    .~ aluOutput
    where (aluOutput, _) = alu cpu

mem :: CPU -> MEMWB
mem cpu = (cpu^.memwb)   & memwbTarget       .~ cpu^.exmem.exmemTarget
                         & memwbRfWriteData  .~ muxMemOut cpu

writeback :: CPU -> WBEND
writeback cpu = (cpu^.wbend)    & wbendTarget       .~ (cpu^.memwb.memwbTarget)
                                & wbendRfWriteData  .~ (cpu^.memwb.memwbRfWriteData)

muxMemOut :: CPU -> Word32
muxMemOut cpu
    | (cpu^.exmem.exmemOp) == Just OPLW     = M.getMemWord (cpu^.memory) (cpu^.exmem.exmemAluOutput)
    | otherwise                             = cpu^.exmem.exmemAluOutput

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
    | cpu^.idex.idexOp == Just OPADD        = muxAlu2Reg cpu
    | cpu^.idex.idexOp == Just OPADDI       = cpu^.idex.idexOperand0
    | cpu^.idex.idexOp == Just OPNAND       = muxAlu2Reg cpu
    | cpu^.idex.idexOp == Just OPLUI        = cpu^.idex.idexOperand0
    | cpu^.idex.idexOp == Just OPSW         = cpu^.idex.idexOperand0
    | cpu^.idex.idexOp == Just OPLW         = cpu^.idex.idexOperand0
    | cpu^.idex.idexOp == Just OPBEQ        = muxAlu2Reg cpu
    | cpu^.idex.idexOp == Just OPJALR       = (cpu^.idex.idexPc) + 1
    | otherwise                             = 0

funcAlu :: CPU -> FuncAlu
funcAlu cpu
    | cpu^.idex.idexOp == Just OPADD        = AluADD
    | cpu^.idex.idexOp == Just OPADDI       = AluADD
    | cpu^.idex.idexOp == Just OPNAND       = AluNAND
    | cpu^.idex.idexOp == Just OPLUI        = AluIMM
    | cpu^.idex.idexOp == Just OPSW         = AluADD
    | cpu^.idex.idexOp == Just OPLW         = AluADD
    | cpu^.idex.idexOp == Just OPBEQ        = AluEQ
    | cpu^.idex.idexOp == Just OPJALR       = AluIMM
    | otherwise                             = AluIMM -- ?

alu :: CPU -> (Word32, Bool)
alu cpu = (output, muxAlu1 cpu == muxAlu2Imm cpu)
    where func = funcAlu cpu
          output = case func of
                    AluADD  -> muxAlu1 cpu + muxAlu2Imm cpu
                    AluNAND -> complement $ muxAlu1 cpu .&. muxAlu2Imm cpu
                    AluEQ   -> 0
                    AluIMM  -> muxAlu2Imm cpu

stomp :: CPU -> Bool
stomp cpu = (eq && (cpu^.idex.idexOp == Just OPBEQ)) || (cpu^.idex.idexOp == Just OPJALR)
    where (_, eq) = alu cpu

muxPc :: CPU -> Word32
muxPc cpu
    | eq && (cpu^.idex.idexOp) == Just OPBEQ    = (cpu^.idex.idexPc) + 1 + (cpu^.idex.idexOperand0)
    | (cpu^.idex.idexOp) == Just OPJALR         = muxAlu1 cpu
    | otherwise                                 = (cpu^.begif.begifPc) + 1
    where (_, eq) = alu cpu

stall :: CPU -> Bool
stall cpu = ((cpu^.idex.idexOp) == Just OPLW) && clash
    where clash = case do
                    target <- cpu^.idex.idexTarget
                    ins <- cpu^.ifid.ifidInstruction
                    let reads = readsRegisters ins
                    return (target `elem` reads)
                  of
                    (Just result) -> result
                    Nothing -> False