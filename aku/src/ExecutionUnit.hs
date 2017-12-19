{-# LANGUAGE TemplateHaskell #-}
module ExecutionUnit where

import Control.Lens
import qualified Data.Map.Strict as Map
import Data.Word

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
toEUInstruction (I.SW (RSOperand s1) (RSOperand s2) i) = Just $ I.SW s1 s2 i
toEUInstruction (I.LW _ (RSOperand s) i) = Just $ I.LW () s i
toEUInstruction (I.BEQ (RSOperand s1) (RSOperand s2) i) = Just $ I.BEQ s1 s2 i
toEUInstruction (I.BLT (RSOperand s1) (RSOperand s2) i) = Just $ I.BLT s1 s2 i
toEUInstruction (I.JALR _ (RSOperand s)) = Just $ I.JALR () s
toEUInstruction I.HALT = Just I.HALT
toEUInstruction _ = Nothing

popRSV :: RSVType -> RSVs -> (RSVs, Maybe (EUInstruction, ROBId))
popRSV t rsvs = case readyRsvs of
        []                  -> (rsvs, Nothing)
        (k,(insn, robId)):_ -> ( Map.adjust (const Nothing) k rsvs
                               , Just (insn, robId))
    where
        fn :: Map.Map a (RSVInstruction, ROBId) -> Map.Map a (EUInstruction, ROBId)
        fn = Map.mapMaybe (\(insn, robId) -> do
                           insn' <- toEUInstruction insn
                           return (insn', robId))
        readyRsvs :: [(RSVId, (EUInstruction, ROBId))]
        readyRsvs = Map.toList $
            ( fn
            . Map.filterWithKey (\i _ -> rsvIdHasType t i)
            . Map.filter (\(insn, _) -> readyForDispatch insn)
            . Map.mapMaybe id) rsvs

hasFreeEU :: RSVType -> EUs -> Bool
hasFreeEU t eus = fst eusOfType < length (snd eusOfType)
    where eusOfType = eus Map.! t

makeEU :: (EUInstruction, ROBId) -> EU
makeEU (insn, robId) = EU latency insn robId
    where latency = 2

pushEU :: RSVType -> (EUInstruction, ROBId) -> EUs -> EUs
pushEU t contents eus = if hasFreeEU t eus then
    Map.adjust (\(max, l) -> (max, l ++ [makeEU contents])) t eus
    else eus
