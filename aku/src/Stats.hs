{-# LANGUAGE TemplateHaskell #-}
module Stats where

import Control.Lens

data Stats = Stats {
      _statsCycles :: Int
    , _statsInstructions :: Int
    , _statsMispredictions :: Int
    , _statsBranches :: Int
    , _statsDispatched :: Int
} deriving (Eq, Show)

makeLenses ''Stats

instance Monoid Stats where
    mempty = Stats 0 0 0 0 0
    Stats c i m b d `mappend` Stats c' i' m' b' d' = Stats (c+c') (i+i') (m+m') (b+b') (d+d')

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