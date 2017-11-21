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
        Step    -> return $ fst $ runWriter (update cpu) -- TODO: keep logs
        Quit    -> return cpu