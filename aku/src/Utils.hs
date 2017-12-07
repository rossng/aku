module Utils where

import CPU
import qualified Memory as M
import Stats

import Control.Lens
import Control.Monad.Writer
import Control.Monad.Loops

cpuWithProgram :: CPU -> M.Program -> CPU
cpuWithProgram c p = c & program .~ p

initialState :: CPU -> Writer Stats CPU
initialState cpu = writer (cpu, mempty)

executeProgramToStep :: CPU -> M.Program -> Int -> (CPU, Stats)
executeProgramToStep c p s = runWriter $ repeatFunction s update (cpuWithProgram c p)

executeProgramUntilHalt :: CPU -> M.Program -> (CPU, Stats)
executeProgramUntilHalt c p = executeUntilHalt (cpuWithProgram c p)

executeUntilHalt :: CPU -> (CPU, Stats)
executeUntilHalt c = runWriter $ iterateUntilM (^.halted) update c

repeatFunction :: Monad m => Int -> (a -> m a) -> (a -> m a)
repeatFunction n f = foldr (<=<) return (replicate n f)

