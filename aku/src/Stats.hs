{-# LANGUAGE TemplateHaskell #-}
module Stats where

import Control.Lens

data Stats = Stats {
      _statsCycles :: Int
    , _statsInstructions :: Int
    , _statsMispredictions :: Int
    , _statsDispatched :: Int
} deriving (Eq, Show)

makeLenses ''Stats

instance Monoid Stats where
    mempty = Stats 0 0 0 0
    Stats c i m d `mappend` Stats c' i' m' d' = Stats (c+c') (i+i') (m+m') (d+d')

emptyStats :: Stats
emptyStats = mempty

oneCycle :: Stats
oneCycle = emptyStats & statsCycles .~ 1

oneDispatch :: Stats
oneDispatch = emptyStats & statsDispatched .~ 1