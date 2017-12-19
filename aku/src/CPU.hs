{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module CPU where

import qualified Data.Vector as V
import Data.Word
import Data.Int
import Data.List
import Control.Lens
import Data.Bits
import Data.Maybe
import Control.Monad.Writer
import qualified Data.Map.Strict as Map

import Registers
import Instruction
import qualified Memory as M
import ReservationStation
import RegisterStatusTable
import Stats
import ExecutionUnit

initialRST :: RST
initialRST = Map.fromList [
      (X0, Nothing)
    , (X1, Nothing)
    , (X2, Nothing)
    , (X3, Nothing)
    , (X4, Nothing)
    , (X5, Nothing)
    , (X6, Nothing)
    , (X7, Nothing)
    ]

initialReorderBuffer :: ROB
initialReorderBuffer = ROB {
      _robFilled = []
    , _robEmpty = [1..size]
    } where size = 16

initialRSVs :: RSVs
initialRSVs = makeRSVs QuickInt 2
    `Map.union` makeRSVs LoadStore 2
    `Map.union` makeRSVs SlowInt 2
    `Map.union` makeRSVs Branch 1

initialEUs :: EUs
initialEUs =    makeEUs QuickInt 1
    `Map.union` makeEUs LoadStore 1
    `Map.union` makeEUs SlowInt 1
    `Map.union` makeEUs Branch 1

data CPU = CPU {
      _program :: M.Program
    , _memory :: M.Memory
    , _registers :: Registers
    , _halted :: Bool
    , _pc :: Int
    , _rst :: RST
    , _rob :: ROB
    , _rsv :: RSVs
    , _unresolvedBranch :: Bool
    , _executionUnits :: EUs
} deriving (Eq)

makeLenses ''CPU

instance Show CPU where
    show cpu = show (cpu^.program) ++ "\n" ++
               show (cpu^.memory) ++ "\n" ++
               show (cpu^.registers) ++ "\n" ++
               (if cpu^.halted then "HALTED" else "ACTIVE") ++ "\n" ++
               show (cpu^.pc) ++ "\n" ++
               show (cpu^.rst) ++ "\n" ++
               show (cpu^.rob) ++ "\n" ++
               show (cpu^.rsv)

initialCPU :: CPU
initialCPU = CPU M.emptyProgram M.emptyMemory emptyRegisters False 0 initialRST initialReorderBuffer initialRSVs False initialEUs

update :: CPU -> Writer Stats CPU
update cpu = writer (cpu', Stats 1)
  where
    cpu' = if cpu^.halted then cpu else cpu

opToRSVType :: Opcode -> RSVType
opToRSVType OPADD = QuickInt
opToRSVType OPADDI = QuickInt
opToRSVType OPNAND = QuickInt
opToRSVType OPSW = LoadStore
opToRSVType OPLW = LoadStore
opToRSVType OPBEQ = Branch
opToRSVType OPBLT = Branch
opToRSVType OPJALR = Branch
opToRSVType OPHALT = Branch

dispatch :: CPU -> CPU
dispatch cpu = if cpu^.unresolvedBranch
    then cpu
    else case slots of
        Nothing -> cpu
        Just (rsvId, robId) -> case rsvType of
            QuickInt    -> dispatchNormal rsvId robId insn cpu
            Branch      -> dispatchNormal rsvId robId insn cpu & unresolvedBranch .~ True
            LoadStore   -> dispatchNormal rsvId robId insn cpu
        where
            insn = M.getInstruction (cpu^.program) (cpu^.pc)
            op = insToOp insn
            rsvType = opToRSVType op
            slots = findEmptySlots rsvType (cpu^.rsv) (cpu^.rob)


findEmptySlots :: RSVType -> RSVs -> ROB -> Maybe (RSVId, ROBId)
findEmptySlots t rsvs rob = do
    rsvId <- findEmptyRsv t rsvs
    robId <- nextSlot rob
    return (rsvId, robId)

findEmptyRsv :: RSVType -> RSVs -> Maybe RSVId
findEmptyRsv t rsvs = case emptyRsvsOfType of
        [] -> Nothing
        (i,_):_ -> Just i
    where rsvsOfType = Map.filterWithKey (\(RSVId t' _) _ -> t == t') rsvs
          emptyRsvsOfType = Map.toList $ Map.filter (== Nothing) rsvsOfType

makeRSVInstruction :: CPU -> Instruction -> RSVInstruction
makeRSVInstruction cpu insn = res
    where
        res = case insn of
            ADD d s1 s2     -> ADD () s1' s2'
            ADDI d s1 i     -> ADDI () s1' i
            NAND d s1 s2    -> NAND () s1' s2'
            SW s1 s2 i      -> SW s1' s2' i
            LW d s1 i       -> LW () s1' i
            BEQ s1 s2 i     -> BEQ s1' s2' i
            BLT s1 s2 i     -> BLT s1' s2' i
            JALR d s1       -> JALR () s1'
            HALT            -> HALT
        s1' = case source1 insn of
            -- if the RST does not have an entry for rs, get register value directly
            Nothing -> RSNoRegister
            Just rs -> case (cpu^.rst) Map.! rs of
                Nothing -> RSOperand $ readRegister (cpu^.registers) rs
                Just i  -> RSROB i
        s2' = case source2 insn of
            -- if the RST does not have an entry for rs, get register value directly
            Nothing -> RSNoRegister
            Just rs -> case (cpu^.rst) Map.! rs of
                Nothing -> RSOperand $ readRegister (cpu^.registers) rs
                Just i  -> RSROB i

makeROBEntry :: CPU -> Instruction -> ROBEntry
makeROBEntry cpu insn = result
    where
        result = case insn of
            ADD d s1 s2     -> ROBOperation d Nothing
            ADDI d s1 i     -> ROBOperation d Nothing
            NAND d s1 s2    -> ROBOperation d Nothing
            SW s1 s2 i      -> ROBStore Nothing Nothing
            LW d s1 i       -> ROBLoad d Nothing
            BEQ s1 s2 i     -> ROBBranch Nothing Nothing
            BLT s1 s2 i     -> ROBBranch Nothing Nothing
            JALR d s1       -> ROBBranch Nothing (Just True)
            HALT            -> ROBHalt

dispatchNormal :: RSVId -> ROBId -> Instruction -> CPU -> CPU
dispatchNormal rsvId robId insn cpu =
    cpu & rsv %~ Map.adjust (const $ Just (rsvInsn, robId)) rsvId
        & rob %~ enqueue robEntry
        & rst %~ (case dest of
            Nothing -> id
            Just d  -> Map.adjust (const $ Just robId) d)
        & pc %~ (+1)
    where dest = writesRegister insn
          rsvInsn = makeRSVInstruction cpu insn
          robEntry = makeROBEntry cpu insn

issue :: CPU -> CPU
issue = issueBranch . issueLoadStore . issueQuickInt


issueQuickInt :: CPU -> CPU
issueQuickInt cpu = if hasFreeEU QuickInt (cpu^.executionUnits) then cpu' else cpu
    where cpu' = cpu & rsv .~ rsv'
                     & executionUnits %~ case maybeContents of
                        Nothing -> id
                        Just contents -> pushEU QuickInt contents
          (rsv', maybeContents) = popRSV QuickInt (cpu^.rsv)

issueLoadStore :: CPU -> CPU
issueLoadStore cpu = undefined

issueBranch :: CPU -> CPU
issueBranch cpu = undefined