{-# LANGUAGE OverloadedLists #-}
module Main where

import Control.Lens

import CPU
import qualified Memory as M

main :: IO ()
main = print $ last $ executeProgram M.program

--runProgram p = converge update (initialCPU & program .~ p)

executeProgram :: M.Program -> [CPU]
executeProgram p = iterate update (initialCPU & program .~ p)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)