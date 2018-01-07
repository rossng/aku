{-# LANGUAGE TemplateHaskell #-}
module Stats where

import Control.Lens
import Text.Printf

data Stats = Stats {
      _statsCycles :: Int
    , _statsInstructions :: Int
    , _statsMispredictions :: Int
    , _statsBranches :: Int
    , _statsDispatched :: Int
} deriving (Eq)

makeLenses ''Stats

instance Show Stats where
  show s = "Instructions/Cycles: " ++ show (s^.statsInstructions) ++ "/" ++ show (s^.statsCycles)
    ++ "(IPC " ++ printf "%.2f" (ipc s) ++ "), "
    ++ "Mispredictions/Branches: " ++ show (s^.statsMispredictions) ++ "/" ++ show (s^.statsBranches)
    ++ "(Rate " ++ printf "%.2f" (mispredictRate s) ++ "), "
    ++ "Instructions dispatched: " ++ show (s^.statsDispatched)

instance Monoid Stats where
  mempty = Stats 0 0 0 0 0
  Stats c i m b d `mappend` Stats c' i' m' b' d' = Stats (c+c') (i+i') (m+m') (b+b') (d+d')

ipc :: Stats -> Float
ipc s = (fromIntegral (s^.statsInstructions)) / (fromIntegral (s^.statsCycles))

mispredictRate :: Stats -> Float
mispredictRate s = (fromIntegral (s^.statsMispredictions)) / (fromIntegral (s^.statsBranches))

emptyStats :: Stats
emptyStats = mempty

oneCycle :: Stats
oneCycle = emptyStats & statsCycles .~ 1

oneDispatch :: Stats
oneDispatch = emptyStats & statsDispatched .~ 1

oneInstruction :: Stats
oneInstruction = emptyStats & statsInstructions .~ 1

oneBranch :: Stats
oneBranch = emptyStats & statsBranches .~ 1

oneMisprediction :: Stats
oneMisprediction = emptyStats & statsMispredictions .~ 1