{-# LANGUAGE TemplateHaskell #-}
module ExecutionUnit where

import Control.Lens
import qualified Data.Map.Strict as Map
import Data.Word
import Control.Arrow
import Debug.Trace

import ReservationStation
import Instruction as I

type EUInstruction = I.BaseInstruction () Word32 I.UnsignedImmediate I.SignedImmediate

type EUs = Map.Map RSVType (Int, [EU])

data EU = EU {
      _euStatus :: Int
    , _euInstruction:: EUInstruction
    , _euROBId:: ROBId
} deriving (Show, Eq)

makeLenses ''EU

makeEUs :: RSVType -> Int -> EUs
makeEUs t n = Map.fromList [(t, (n, []))]

toEUInstruction :: RSVInstruction -> Maybe EUInstruction
toEUInstruction (I.ADD _ (RSOperand s1) (RSOperand s2)) = Just $ I.ADD () s1 s2
toEUInstruction (I.ADDI _ (RSOperand s) i) = Just $ I.ADDI () s i
toEUInstruction (I.NAND _ (RSOperand s1) (RSOperand s2)) = Just $ I.NAND () s1 s2
toEUInstruction (I.MUL _ (RSOperand s1) (RSOperand s2)) = Just $ I.MUL () s1 s2
toEUInstruction (I.SW (RSOperand s1) (RSOperand s2) i) = Just $ I.SW s1 s2 i
toEUInstruction (I.LW _ (RSOperand s) i) = Just $ I.LW () s i
toEUInstruction (I.BEQ (RSOperand s1) (RSOperand s2) i) = Just $ I.BEQ s1 s2 i
toEUInstruction (I.BLT (RSOperand s1) (RSOperand s2) i) = Just $ I.BLT s1 s2 i
toEUInstruction (I.JALR _ (RSOperand s)) = Just $ I.JALR () s
toEUInstruction I.HALT = Just I.HALT
toEUInstruction _ = Nothing

popRSV :: RSVType -> RSVs -> ROB -> (RSVs, Maybe (EUInstruction, ROBId))
popRSV t rsvs rob = case readyRsvs of
        []                  -> (rsvs, Nothing)
        (k,(insn, robId)):_ -> ( Map.adjust (const Nothing) k rsvs
                               , Just (insn, robId))
    where
        readyRsvs :: [(RSVId, (EUInstruction, ROBId))]
        readyRsvs = Map.toList $
            ( Map.mapMaybe (\(insn, robId) -> do
                 insn' <- toEUInstruction insn
                 return (insn', robId))
            . Map.filterWithKey (\i a -> not (rsvIdHasType Load i) || rsvClashesPendingStores a rob)
            . Map.filterWithKey (\i _ -> rsvIdHasType t i)
            . Map.filter (\(insn, _) -> readyForDispatch insn)
            . Map.mapMaybe id) rsvs

hasFreeEU :: RSVType -> EUs -> Bool
hasFreeEU t eus = fst eusOfType > length (snd eusOfType)
    where eusOfType = eus Map.! t

makeEU :: (EUInstruction, ROBId) -> EU
makeEU (insn, robId) = EU latency insn robId
    where latency = case opToRSVType (insToOp insn) of
                        --Load      -> 10
                        Address   -> 1
                        QuickInt  -> 1
                        SlowInt   -> 4
                        Branch    -> 1

loadLatency :: Int
loadLatency = 3

pushEU :: RSVType -> (EUInstruction, ROBId) -> EUs -> EUs
pushEU t contents eus = if hasFreeEU t eus then
    Map.adjust (\(max, l) -> (max, l ++ [makeEU contents])) t eus
    else eus

pushLoadEU :: (EUInstruction, ROBId) -> EUs -> EUs
pushLoadEU (insn, robId) eus = if hasFreeEU Load eus then
    Map.adjust (\(max, l) -> (max, l ++ [EU loadLatency insn robId])) Load eus
    else eus

stepEU :: EU -> EU
stepEU eu = eu & euStatus -~ 1

stepEUs :: EUs -> EUs
stepEUs = Map.map (second stepEUGroup)
    where stepEUGroup :: [EU] -> [EU]
          stepEUGroup = map stepEU

emptyEUs :: (Int, [EU]) -> (Int, [EU])
emptyEUs (max, eus) = (max, eus')
    where eus' = filter (\eu -> eu^.euStatus > 0) eus

dropUpTo :: Int -> (a -> Bool) -> [a] -> [a]
dropUpTo n p [] = []
dropUpTo 0 p xs = xs
dropUpTo n p (x:xs) = if p x then dropUpTo (n-1) p xs else x : dropUpTo n p xs

emptyAddressEUs :: Int -> (Int, [EU]) -> (Int, [EU])
emptyAddressEUs n (max, eus) = (max, eus'')
    where eus' = dropUpTo n (\eu -> eu^.euStatus <= 0 && insToOp (eu^.euInstruction) == OPLW) eus
          eus'' = filter (\eu -> eu^.euStatus > 0 || insToOp (eu^.euInstruction) == OPLW) eus'

emptyCompleteEUs :: EUs -> EUs
emptyCompleteEUs eus = refilled
    where loadEUs = eus Map.! Load
          freeBranchEUs = fst loadEUs - length (snd loadEUs)
          emptied = Map.mapWithKey (\k a -> if k == Address then emptyAddressEUs freeBranchEUs a else emptyEUs a) eus
          addressEUs = eus Map.! Address
          completedAddressEUs = map (\eu -> (eu^.euInstruction, eu^.euROBId)) $ filter (\eu -> eu^.euStatus <= 0) (snd addressEUs)
          progressableAddressEUs = take freeBranchEUs (filter (\(ins, _) -> insToOp ins == OPLW) completedAddressEUs)
          refilled = foldr pushLoadEU emptied progressableAddressEUs
