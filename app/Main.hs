{-# LANGUAGE OverloadedLists #-}
module Main where

import CPU
import Utils
import qualified Memory as M

main :: IO ()
main = print $ last $ executeProgram initialCPU M.program

--runProgram p = converge update (initialCPU & program .~ p)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)