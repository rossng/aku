module ReservationStation where

import Control.Lens
import Data.Word
import Instruction as I
import Registers as R

data RSVType = LoadStore | QuickInt | SlowInt | Branch deriving (Eq, Show, Ord)
data RSVId = RSVId RSVType Int deriving (Eq, Show, Ord)

data RSSource =
--      RSRegister R.RegisterName
      RSRSV RSVId
    | RSOperand Word32
    | RSNoRegister
    deriving (Eq, Show)

type RSVInstruction = I.BaseInstruction () RSSource I.UnsignedImmediate I.SignedImmediate

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