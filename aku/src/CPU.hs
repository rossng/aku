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

initialROB :: ROB
initialROB = ROB {
      _robFilled = []
    , _robEmpty = [1..size]
    } where size = 16

initialRSVs :: RSVs
initialRSVs = makeRSVs QuickInt 2
    `Map.union` makeRSVs SlowInt 2
    `Map.union` makeRSVs Branch 1
    `Map.union` makeRSVs Address 2
    `Map.union` makeRSVs Load 2

initialEUs :: EUs
initialEUs =    makeEUs QuickInt 1
    `Map.union` makeEUs SlowInt 1
    `Map.union` makeEUs Branch 1
    `Map.union` makeEUs Address 1
    `Map.union` makeEUs Load 1

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
               (if cpu^.unresolvedBranch then "UNRESOLVED" else "RESOLVED") ++ "\n" ++
               "PC:     " ++ show (cpu^.pc) ++ "\n" ++
               "RST:    " ++ show (cpu^.rst) ++ "\n" ++
               "ROB:    " ++ show (cpu^.rob) ++ "\n" ++
               "RSVs:   " ++ show (cpu^.rsv) ++ "\n" ++
               "EUs:    " ++ show (cpu^.executionUnits)

initialCPU :: CPU
initialCPU = CPU M.emptyProgram M.emptyMemory emptyRegisters False 0 initialRST initialROB initialRSVs False initialEUs

update :: CPU -> Writer Stats CPU
update cpu = writer (cpu', Stats 1)
  where
    cpu' = if cpu^.halted
           then cpu
           else (issue.execute.dispatch.commit) cpu

dispatch :: CPU -> CPU
dispatch cpu = case M.safeGetInstruction (cpu^.program) (cpu^.pc) of
    Just insn ->
        case slots of
            Nothing -> cpu
            Just (rsvId, robId) -> case rsvType of
                QuickInt    -> dispatchNormal rsvId robId insn cpu
                Branch      -> if cpu^.unresolvedBranch then cpu else dispatchNormal rsvId robId insn cpu & unresolvedBranch .~ True
                Address     -> dispatchNormal rsvId robId insn cpu
                Load        -> dispatchNormal rsvId robId insn cpu
            where
                op = insToOp insn
                rsvType = opToRSVType op
                slots = findEmptySlots rsvType (cpu^.rsv) (cpu^.rob)
    Nothing -> cpu


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
            MUL d s1 s2     -> MUL () s1' s2'
            NAND d s1 s2    -> NAND () s1' s2'
            SW s1 s2 i      -> SW s1' s2' i
            LW d s1 i       -> LW () s1' i
            BEQ s1 s2 i     -> BEQ s1' s2' i
            BLT s1 s2 i     -> BLT s1' s2' i
            JALR d s1       -> JALR () s1'
            HALT            -> HALT
        s1' = case sourceReg1 insn of
            -- if the RST does not have an entry for rs, get register value directly
            Nothing -> RSNoRegister
            Just rs -> case (cpu^.rst) Map.! rs of
                Nothing -> RSOperand $ readRegister (cpu^.registers) rs
                Just i  -> RSROB i
        s2' = case sourceReg2 insn of
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
            MUL d s1 s2     -> ROBOperation d Nothing
            NAND d s1 s2    -> ROBOperation d Nothing
            SW s1 s2 i      -> ROBStore Nothing Nothing
            LW d s1 i       -> ROBLoad d Nothing Nothing
            BEQ s1 s2 i     -> ROBBranch (cpu^.pc) False Nothing Nothing
            BLT s1 s2 i     -> ROBBranch (cpu^.pc) False Nothing Nothing
            JALR d s1       -> ROBBranch (cpu^.pc) False Nothing (Just True)
            HALT            -> ROBHalt False

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
issue = issueTo Branch . issueTo Load . issueTo Address . issueTo QuickInt

issueTo :: RSVType -> CPU -> CPU
issueTo t cpu = if hasFreeEU t (cpu^.executionUnits) then cpu' else cpu
    where cpu' = cpu & rsv .~ rsv'
                     & executionUnits %~ case maybeContents of
                        Nothing -> id
                        Just contents -> pushEU t contents
          (rsv', maybeContents) = popRSV t (cpu^.rsv) (cpu^.rob)


-- given the contents of the ROB and an execution unit, if the EU is finished,
-- compute the update to be published to the CDB and any changes to the ROB entry
getEUResult :: CPU -> EU -> (ROBEntry, Maybe Word32)
getEUResult cpu eu = if eu^.euStatus > 0
    then (robEntry, Nothing)
    else case (eu^.euInstruction, robEntry) of
    (ADD () s1 s2, ROBOperation d r)
        -> (ROBOperation d (Just (s1 + s2)), Just (s1 + s2))
    (ADDI () s (ImmS i), ROBOperation d r)
        -> (ROBOperation d (Just (s + fromIntegral i)), Just (s + fromIntegral i))
    (MUL () s1 s2, ROBOperation d r)
        -> (ROBOperation d (Just (s1 * s2)), Just (s1 * s2))
    (NAND () s1 s2, ROBOperation d r)
        -> (ROBOperation d (Just (complement (s1 .&. s2))), Just (complement (s1 .&. s2)))
    (SW s1 s2 (ImmS i), ROBStore d r)
        -> (ROBStore (Just (fromIntegral s2 + fromIntegral i)) (Just s1), Nothing)
    (LW () s (ImmS i), ROBLoad d Nothing Nothing)
        -> (ROBLoad d (Just (fromIntegral s + fromIntegral i)) Nothing, Nothing)
    (LW () s (ImmS i), ROBLoad d (Just addr) Nothing)
        -> (ROBLoad d (Just addr) (Just (M.getMemWord addr mem)), Just (M.getMemWord addr mem))
    (BEQ s1 s2 (ImmS i), ROBBranch pc' pred Nothing Nothing)
        -> (ROBBranch pc' pred (Just (pc' + 1 + fromIntegral i)) (Just (s1 == s2)), Nothing)
    (BLT s1 s2 (ImmS i), ROBBranch pc' pred Nothing Nothing)
        -> (ROBBranch pc' pred (Just (pc' + 1 + fromIntegral i)) (Just (s1 < s2)), Nothing)
    (JALR () s, ROBBranch pc' pred d r)
        -> (ROBBranch pc' pred (Just (fromIntegral s)) r, Nothing)
    (HALT, ROBHalt _)
        -> (ROBHalt True, Nothing)
    where robEntry = getROBEntry (cpu^.rob) (eu^.euROBId)
          mem = cpu^.memory


getEUResults :: CPU -> [(ROBId, ROBEntry, Maybe Word32)]
getEUResults cpu = zipWith (\a (b,c) -> (a,b,c)) (map (^.euROBId) completableEUs) (map (getEUResult cpu) completableEUs)
    where completableEUs =
            ( filter (\e -> e^.euStatus <= 0)
            . concatMap snd
            . Map.toList
            . Map.map snd )
            (cpu^.executionUnits)

completeEUs :: CPU -> CPU
completeEUs cpu = cpu   & rob .~ foldr (\(robId, robEntry, _) r -> updateROBEntry robId robEntry r) (cpu^.rob) euResults
                        & rsv .~ foldr (\(robId, _, result) r -> updateRSVs robId result r) (cpu^.rsv) justEuResults
                        & executionUnits %~ emptyCompleteEUs
    where euResults = getEUResults cpu
          justEuResults = mapMaybe (\(robId, robEntry, result) -> do { result' <- result; return (robId, robEntry, result') }) euResults

execute :: CPU -> CPU
execute cpu = completeEUs $ cpu & executionUnits %~ stepEUs

-- todo: correct offset for branches, speculative execution
commitROBEntry :: ROBEntry -> CPU -> CPU
commitROBEntry robEntry cpu = case robEntry of
    ROBLoad (Dest dest) (Just _) (Just value)
        -> cpu & registers %~ writeRegister dest value
               & rst %~ Map.adjust (const Nothing) dest
    ROBStore (Just addr) (Just value)
        -> cpu & memory %~ M.setMemWord addr value
    ROBOperation (Dest dest) (Just value)
        -> cpu & registers %~ writeRegister dest value
               & rst %~ Map.adjust (const Nothing) dest
    ROBBranch _ pred (Just addr) (Just taken)
        -> if pred == taken
           then cpu & pc .~ (if taken then addr else cpu^.pc)
                    & unresolvedBranch .~ False
           else cpu & pc .~ (if taken then addr else cpu^.pc)
                    & unresolvedBranch .~ False
                    & rst .~ initialRST
                    & rob .~ initialROB
                    & rsv .~ initialRSVs
                    & executionUnits .~ initialEUs
    ROBHalt True
        -> cpu & halted .~ True
    _   -> cpu

commit :: CPU -> CPU
commit cpu = cpu'
    where (rob', robEntry) = dequeue (cpu^.rob)
          cpu' = case robEntry of
                     Just entry  -> commitROBEntry entry (cpu & rob .~ rob')
                     Nothing     -> cpu & rob .~ rob'