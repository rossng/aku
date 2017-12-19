module RegisterStatusTable where

import qualified Data.Map.Strict as Map

import Registers as R
import ReservationStation

type RST = Map.Map R.RegisterName (Maybe ROBId)