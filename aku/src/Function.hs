module Function where

import Control.Monad.Writer
import Control.Monad.Loops

repeatFunction :: Monad m => Int -> (a -> m a) -> (a -> m a)
repeatFunction n f = foldr (<=<) return (replicate n f)