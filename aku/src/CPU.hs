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

makeRSVs :: RSVType -> Int -> Map.Map RSVId (Maybe RSVInstruction)
makeRSVs t n = Map.fromList $ map (\i -> (RSVId t i, Nothing)) [1..n]

initialLoadStoreQueue :: LSQ
initialLoadStoreQueue = LSQ {
      _lsqQueued = []
    , _lsqUnqueued = map (RSVId LoadStore) [1..size]
    } where size = 4

initialRSVs :: Map.Map RSVId (Maybe RSVInstruction)
initialRSVs = makeRSVs QuickInt 2 `Map.union` makeRSVs SlowInt 2 `Map.union` makeRSVs Branch 1

data CPU = CPU {
      _program :: M.Program
    , _memory :: M.Memory
    , _registers :: Registers
    , _halted :: Bool
    , _pc :: Int
    , _rst :: RST
    , _loadStoreQueue :: LSQ
    , _rsv :: Map.Map RSVId (Maybe RSVInstruction)
    , _unresolvedBranch :: Bool
} deriving (Eq)

makeLenses ''CPU

instance Show CPU where
    show cpu = show (cpu^.program) ++ "\n" ++
               show (cpu^.memory) ++ "\n" ++
               show (cpu^.registers) ++ "\n" ++
               (if cpu^.halted then "HALTED" else "ACTIVE") ++ "\n" ++
               show (cpu^.pc) ++ "\n" ++
               show (cpu^.rst) ++ "\n" ++
               show (cpu^.loadStoreQueue) ++ "\n" ++
               show (cpu^.rsv)

initialCPU :: CPU
initialCPU = CPU M.emptyProgram M.emptyMemory emptyRegisters False 0 initialRST initialLoadStoreQueue initialRSVs False

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
dispatch cpu = case rsvIdx of
    Nothing -> cpu
    Just idx -> case rsvType of
        QuickInt    -> dispatchNormal idx insn cpu
        Branch      -> dispatchNormal idx insn cpu & unresolvedBranch .~ True
        LoadStore   -> dispatchLoadStore idx insn cpu
    where insn = M.getInstruction (cpu^.program) (cpu^.pc)
          op = insToOp insn
          rsvType = opToRSVType op
          rsvIdx = findEmptyRsv rsvType cpu

findEmptyRsv :: RSVType -> CPU -> Maybe RSVId
findEmptyRsv t cpu = case t of
    LoadStore   -> undefined -- TODO get next slot in lsq
    _           -> case emptyRsvsOfType of
        [] -> Nothing
        (i,_):_ -> Just i
    where rsvsOfType = Map.filterWithKey (\(RSVId t' _) _ -> t == t') (cpu^.rsv)
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
                Just i  -> RSRSV i
        s2' = case source2 insn of
            -- if the RST does not have an entry for rs, get register value directly
            Nothing -> RSNoRegister
            Just rs -> case (cpu^.rst) Map.! rs of
                Nothing -> RSOperand $ readRegister (cpu^.registers) rs
                Just i  -> RSRSV i

dispatchNormal :: RSVId -> Instruction -> CPU -> CPU
dispatchNormal rsvId insn cpu =
    cpu & rsv %~ Map.adjust (const $ Just $ makeRSVInstruction cpu insn) rsvId
        & rst %~ (case dest of
            Nothing -> id
            Just d  -> Map.adjust (const $ Just rsvId) d)
    where dest = writesRegister insn


dispatchLoadStore :: RSVId -> Instruction -> CPU -> CPU
dispatchLoadStore = undefined