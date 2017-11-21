{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import CPU

type Parser = Parsec Void String

data Command =  Reset -- resets the CPU to initialCPU

main :: IO ()
main = do   putStrLn "aku debugger"
            let cpu = initialCPU
            commandString <- getLine
            let command = parseMaybe commandParser commandString
            cpu <- case command of
                    Just cmd    -> return (applyCommand cmd cpu)
                    Nothing     -> do { putStrLn "Command not recognised"; return cpu }
            print cpu

applyCommand :: Command -> CPU -> CPU
applyCommand command cpu = case command of
                                Reset   -> initialCPU

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

commandParser :: Parser Command
commandParser = resetParser <* eof