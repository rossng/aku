{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Int
import Control.Lens

import CPU
import Utils
import qualified Memory as M
import Registers
import Assembler
import Stats

main :: IO ()
main = do
    putStrLn "aku benchmarks"
    maybeProg <- loadProgram "aku-benchmarks/programs/bubble_sort.asm"
    case maybeProg of
        Just prog   -> do
            let (cpu, stats) = benchmarkWithMemory (reverse [1..100]) prog
            print stats
            print cpu
        Nothing     -> putStrLn "Could not load program"


benchmarkWithMemory :: [Int32] -> M.Program -> (CPU, Stats)
benchmarkWithMemory list = executeProgramUntilHalt init
    where init = initialCPU & memory .~ M.setMemWords 0 list M.emptyMemory
                            & (registers.x1) .~ fromIntegral (length list)