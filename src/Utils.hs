module Utils where

import CPU
import qualified Memory as M

import Control.Lens

cpuWithProgram :: CPU -> M.Program -> CPU
cpuWithProgram c p = c & program .~ p

executeProgram :: CPU -> M.Program -> [CPU]
executeProgram c p = iterate update $ cpuWithProgram c p

executeProgramToStep :: CPU -> M.Program -> Int -> CPU
executeProgramToStep c p s = executeProgram c p !! s