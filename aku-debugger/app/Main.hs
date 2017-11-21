{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Void
import Data.Maybe
import Control.Monad
import Control.Monad.Loops
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import CPU

type Parser = Parsec Void String

data Command =  Reset -- resets the CPU to initialCPU
              | Quit
              deriving (Eq, Show)

main :: IO ()
main = do   putStrLn "aku debugger"
            finalCPU <- iterateUntilM ((== Just Quit) . snd) repl (initialCPU, Nothing)
            print finalCPU
            return ()

repl :: (CPU, Maybe Command) -> IO (CPU, Maybe Command)
repl (cpu, command) = do
        print cpu
        commandString <- getLine
        cpu' <- case command of
               Just cmd    -> return (applyCommand cmd cpu)
               Nothing     -> return cpu
        let command' = parseMaybe commandParser commandString
        when (isNothing command') $ putStrLn "Command not recognised"
        return (cpu', command')

applyCommand :: Command -> CPU -> CPU
applyCommand command cpu = case command of
                                Reset   -> initialCPU
                                Quit    -> cpu

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
    where lineCmnt = L.skipLineComment "//"
          blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed space integer

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

quoted :: Parser a -> Parser a
quoted = between (symbol "\"") (symbol "\"")

resetParser :: Parser Command
resetParser =     Reset <$ try (rword "reset")
              <|> Reset <$ try (rword "r")
              <|> Quit <$ try (rword "quit")
              <|> Quit <$ try (rword "q")

commandParser :: Parser Command
commandParser = resetParser <* eof