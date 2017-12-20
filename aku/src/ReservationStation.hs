{-# LANGUAGE TemplateHaskell #-}
module ReservationStation where

import Control.Lens
import Data.Word
import Instruction as I
import Registers as R
import Memory as M
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Control.Arrow

type RSVs = Map.Map RSVId (Maybe (RSVInstruction, ROBId))

data RSVType =  Load | Address | QuickInt | SlowInt | Branch deriving (Eq, Show, Ord)
data RSVId = RSVId RSVType Int deriving (Eq, Show, Ord)

data RSSource =
--      RSRegister R.RegisterName
      RSROB ROBId
    | RSOperand Word32
    | RSNoRegister
    deriving (Eq, Show)

type RSVInstruction = I.BaseInstruction () RSSource I.UnsignedImmediate I.SignedImmediate

rsvIdHasType :: RSVType -> RSVId -> Bool
rsvIdHasType rsvType (RSVId t _) = t == rsvType

makeRSVs :: RSVType -> Int -> RSVs
makeRSVs t n = Map.fromList $ map (\i -> (RSVId t i, Nothing)) [1..n]

readyForDispatch :: RSVInstruction -> Bool
readyForDispatch (I.ADD _ (RSOperand _) (RSOperand _)) = True
readyForDispatch (I.ADDI _ (RSOperand _) _) = True
readyForDispatch (I.NAND _ (RSOperand _) (RSOperand _)) = True
readyForDispatch (I.SW (RSOperand _) (RSOperand _) _) = True
readyForDispatch (I.LW _ (RSOperand _) _) = True
readyForDispatch (I.BEQ (RSOperand _) (RSOperand _) _) = True
readyForDispatch (I.BLT (RSOperand _) (RSOperand _) _) = True
readyForDispatch (I.JALR _ (RSOperand _)) = True
readyForDispatch I.HALT = True
readyForDispatch _ = False

updateRSVs :: ROBId -> Word32 -> RSVs -> RSVs
updateRSVs robId operand = Map.map (fmap $ first (updateRSVInstruction robId operand))

-- replace any ROBId placeholders in a reservation station
-- with the real operand
updateRSVInstruction :: ROBId -> Word32 -> RSVInstruction -> RSVInstruction
updateRSVInstruction robId operand rsvInsn = insn''
    where insn' = case source1 rsvInsn of
                      Just (RSROB r)    -> if r == robId
                        then updateSource1 (RSOperand operand) rsvInsn
                        else rsvInsn
                      _                 -> rsvInsn
          insn'' = case source2 rsvInsn of
                      Just (RSROB r)    -> if r == robId
                        then updateSource2 (RSOperand operand) rsvInsn
                        else rsvInsn
                      _                 -> rsvInsn

readyForCommit :: ROBEntry -> Bool
readyForCommit (ROBLoad _ (Just _) (Just _)) = True
readyForCommit (ROBStore (Just _) (Just _)) = True
readyForCommit (ROBOperation _ (Just _)) = True
readyForCommit (ROBBranch (Just _) (Just _)) = True
readyForCommit ROBHalt = True
readyForCommit _ = False

type ROBId = Int

data ROBEntry =
      ROBLoad I.DestRegister (Maybe Int) (Maybe Word32)
    | ROBStore (Maybe Int) (Maybe Word32)
    | ROBOperation I.DestRegister (Maybe Word32)
    | ROBBranch (Maybe Int) (Maybe Bool)
    | ROBHalt
    deriving (Show, Eq)

isROBStore :: ROBEntry -> Bool
isROBStore e = case e of
    ROBStore{}  -> True
    _           -> False

data ROB = ROB {
      _robFilled :: [(ROBId, ROBEntry)]
    , _robEmpty :: [ROBId]
} deriving (Show, Eq)

makeLenses ''ROB

full :: ROB -> Bool
full rob = null $ rob^.robEmpty

nextSlot :: ROB -> Maybe Int
nextSlot rob = case rob^.robEmpty of
    []  -> Nothing
    s:_ -> Just s

enqueue :: ROBEntry -> ROB -> ROB
enqueue entry rob = case rob^.robEmpty of
    []  -> rob
    s:_ -> rob & robFilled      %~ (++ [(s, entry)])
               & robEmpty       %~ tail

dequeue :: ROB -> (ROB, Maybe ROBEntry)
dequeue rob = case rob^.robFilled of
    []  -> (rob, Nothing)
    s:_ -> (rob & robFilled     %~ tail
                & robEmpty      %~ (++ [fst s]), Just $ snd s)

getROBEntry :: ROB -> ROBId -> ROBEntry
getROBEntry rob robId = snd $ head $ filter (\(i,_) -> i == robId) (rob^.robFilled)

updateROBEntry :: ROBId -> ROBEntry -> ROB -> ROB
updateROBEntry robId robEntry rob = rob & robFilled %~ map fn
    where fn (i, entry) = if i == robId
            then (i, robEntry)
            else (i, entry)

entriesBeforeRobId :: ROBId -> ROB -> [(ROBId, ROBEntry)]
entriesBeforeRobId robId rob = takeWhile (\(i, _) -> i < robId) (rob^.robFilled)

pendingStoresWithAddress :: Int -> ROBId -> ROB -> Bool
pendingStoresWithAddress addr robId rob = any (\(i,e) -> storeToAddr addr e) (entriesBeforeRobId robId rob)
    where storeToAddr :: Int -> ROBEntry -> Bool
          storeToAddr addr e = case e of
            ROBStore (Just a) _ -> a == addr
            _                   -> False

rsvClashesPendingStores :: (RSVInstruction, ROBId) -> ROB -> Bool
rsvClashesPendingStores (rsvInsn, robId) rob = case rsvInsn of
    I.LW _ (RSOperand a) (ImmS o) -> pendingStoresWithAddress (fromIntegral a + fromIntegral o) robId rob
    I.LW{}                        -> True
    _                             -> False
