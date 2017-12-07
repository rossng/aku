{-# LANGUAGE TemplateHaskell #-}
module Stats where

import Control.Lens

data Stats = Stats {
      _statsCycles :: Int
} deriving (Eq, Show)

makeLenses ''Stats

instance Monoid Stats where
    mempty = Stats 0
    Stats a `mappend` Stats b = Stats (a + b)