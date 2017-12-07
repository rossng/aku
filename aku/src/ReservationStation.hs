module ReservationStation where

import Control.Lens
import Data.Void
import Data.Word
import Instruction as I
import Registers as R

data RSVType = LoadStore | QuickInt | SlowInt | Branch deriving (Eq, Show)
data RSVId = RSVId RSVType Int deriving (Eq, Show)

data RSSource =
      RSRegister R.RegisterName
    | RSRSV RSVId
    | RSOperand Word32
    | RSNoRegister
    deriving (Eq, Show)

data RSV = RSV
    RSVId
    (Maybe (I.BaseInstruction Void RSSource I.UnsignedImmediate I.SignedImmediate)) deriving (Eq, Show)

readyForDispatch :: RSV -> Bool
readyForDispatch (RSV _ Nothing) = False
readyForDispatch (RSV _ (Just rs)) = insnReady rs

insnReady :: BaseInstruction Void RSSource I.UnsignedImmediate I.SignedImmediate -> Bool
insnReady (I.ADD _ (RSOperand _) (RSOperand _)) = True
insnReady (I.ADDI _ (RSOperand _) _) = True
insnReady (I.NAND _ (RSOperand _) (RSOperand _)) = True
insnReady (I.SW (RSOperand _) (RSOperand _) _) = True
insnReady (I.LW _ (RSOperand _) _) = True
insnReady (I.BEQ (RSOperand _) (RSOperand _) _) = True
insnReady (I.BLT (RSOperand _) (RSOperand _) _) = True
insnReady (I.JALR _ (RSOperand _)) = True
insnReady I.HALT = True
insnReady _ = False