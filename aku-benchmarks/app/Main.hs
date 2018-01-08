{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import Data.Int
import Control.Lens
import System.Environment

import CPU
import Utils
import qualified Memory as M
import Registers
import Assembler
import Stats

main :: IO ()
main = do
    putStrLn "aku benchmarks"
    args <- getArgs
    let arg = head args
    if | arg == "matrix"  -> do
          maybeProg <- loadProgram "aku-benchmarks/programs/two_matrix_multiply.asm"
          case maybeProg of
              Just prog   -> do
                  let (cpu, stats) = benchmarkWithMemory [1,2,3,4,5,6,7,8] prog
                  print stats
                  print cpu
              Nothing     -> putStrLn "Could not load program"
       | arg == "throughput" -> do
          maybeProg <- loadProgram "aku-benchmarks/programs/throughput.asm"
          case maybeProg of
              Just prog   -> do
                  let (cpu, stats) = benchmarkWithMemory [] prog
                  print stats
                  print cpu
              Nothing     -> putStrLn "Could not load program"
       | arg == "bubblesort" -> do
          maybeProg <- loadProgram "aku-benchmarks/programs/bubble_sort.asm"
          case maybeProg of
              Just prog   -> do
                  let (cpu, stats) = benchmarkWithMemory (reverse [1..100]) prog
                  print stats
                  print cpu
              Nothing     -> putStrLn "Could not load program"
       | arg == "dotproduct" -> do
          maybeProg <- loadProgram "aku-benchmarks/programs/vector_dot_product.asm"
          case maybeProg of
              Just prog   -> do
                  let (cpu, stats) = benchmarkWithCPUAndMemory (initialCPU & (registers.x1) .~ 30) ([1..30] ++ [1..30]) prog
                  print stats
                  print cpu
              Nothing     -> putStrLn "Could not load program"
       | otherwise -> (putStrLn "Invalid program choice")


benchmarkWithMemory :: [Int32] -> M.Program -> (CPU, Stats)
benchmarkWithMemory list = executeProgramUntilHalt init
    where init = initialCPU & memory .~ M.setMemWords 0 list M.emptyMemory
                            & (registers.x1) .~ fromIntegral (length list)

benchmarkWithCPUAndMemory :: CPU -> [Int32] -> M.Program -> (CPU, Stats)
benchmarkWithCPUAndMemory cpu list = executeProgramUntilHalt init
    where init = cpu & memory .~ M.setMemWords 0 list M.emptyMemory