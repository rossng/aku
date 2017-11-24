{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Void
import Data.Maybe
import Data.Char
import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Writer.Lazy

import CPU
import Assembler
import Command
import qualified Memory as M
import Utils

main :: IO ()
main = do
        putStrLn "aku debugger"
        let cpu = initialCPU
        print cpu
        finalCPU <- iterateUntilM ((== Just Quit) . snd) repl (cpu, Nothing)
        return ()

repl :: (CPU, Maybe Command) -> IO (CPU, Maybe Command)
repl (cpu, command) = do
        commandString <- getLine
        let command' = parseCommand commandString
        when (isNothing command') $ putStrLn "Command not recognised"
        cpu' <- case command' of
               Just cmd    -> applyCommand cmd cpu
               Nothing     -> return cpu
        print cpu'
        return (cpu', command')

applyCommand :: Command -> CPU -> IO CPU
applyCommand command cpu = case command of
        Reset   -> return initialCPU
        LoadProgram path -> do
           maybeProgram <- loadProgram path
           case maybeProgram of
                Just p -> return $ cpu & program .~ p
                Nothing -> putStrLn "Failed to load program" >>
                           return cpu
        Step    -> return $ extractWriter (update cpu) -- TODO: keep logs
        StepN n -> return $ extractWriter $ repeatFunction n update cpu
        SetMemory a vs
                -> return $ cpu & memory .~ (M.setMemWords (cpu^.memory) a vs)
        Quit    -> return cpu

extractWriter :: Writer w a -> a
extractWriter = fst . runWriter