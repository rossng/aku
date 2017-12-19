{-# LANGUAGE TemplateHaskell #-}
module ReservationStation where

import Control.Lens
import Data.Word
import Instruction as I
import Registers as R
import Memory as M
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map

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

readyForCommit :: ROBEntry -> Bool
readyForCommit (ROBLoad _ (Just _)) = True
readyForCommit (ROBStore (Just _) (Just _)) = True
readyForCommit (ROBOperation _ (Just _)) = True
readyForCommit (ROBBranch (Just _) (Just _)) = True
readyForCommit ROBHalt = True
readyForCommit _ = False

type ROBId = Int

data ROBEntry =
      ROBLoad I.DestRegister (Maybe Word32)
    | ROBStore (Maybe Int) (Maybe Word32)
    | ROBOperation I.DestRegister (Maybe Word32)
    | ROBBranch (Maybe Int) (Maybe Bool)
    | ROBHalt
    deriving (Show, Eq)

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



storesBefore :: ROBId -> ROB -> [(ROBId, ROBEntry)]
storesBefore robId rob = takeWhile (\(i, _) -> i < robId) (rob^.robFilled)