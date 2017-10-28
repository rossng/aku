module Utils where

import CPU
import qualified Memory as M

import Control.Lens
import Control.Monad.Writer

cpuWithProgram :: CPU -> M.Program -> CPU
cpuWithProgram c p = c & program .~ p

initialState :: CPU -> Writer Stats CPU
initialState cpu = writer (cpu, mempty)

executeProgramToStep :: CPU -> M.Program -> Int -> (CPU, Stats)
executeProgramToStep c p s = runWriter $ do init <- initialState (cpuWithProgram c p)
                                            repeatFunction s update init

repeatFunction :: Monad m => Int -> (a -> m a) -> (a -> m a)
repeatFunction n f = foldr (<=<) return (replicate n f)