{-# LANGUAGE OverloadedLists #-}
module Main where

import CPU
import Utils
import qualified Memory as M

main :: IO ()
main = print $ executeProgramToStep initialCPU M.program 1

--runProgram p = converge update (initialCPU & program .~ p)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)