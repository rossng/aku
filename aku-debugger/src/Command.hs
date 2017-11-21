module Command where

import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Command =  Reset -- resets the CPU to initialCPU
              | LoadProgram String -- load a program into memory
              | Step -- step forward one clock cycle
              | Quit  -- quit the debugger
              deriving (Eq, Show)

infixl <||>
p <||> q = try p <|> try q

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

filePathParser :: Parser String
filePathParser = quoted (some (satisfy (\c -> not (isSpace c) && not (isControl c) && c /= '"')))

resetParser :: Parser Command
resetParser =     Reset <$ try (rword "reset")
              <|> Reset <$ try (rword "r")

stepParser :: Parser Command
stepParser =     Step <$ try (rword "step")
             <|> Step <$ try (rword "s")

quitParser :: Parser Command
quitParser =      Quit <$ try (rword "quit")
              <|> Quit <$ try (rword "q")

loadParser :: Parser Command
loadParser =      LoadProgram <$ try (rword "load") <*> filePathParser
              <|> LoadProgram <$ try (rword "l") <*> filePathParser

commandParser :: Parser Command
commandParser = (    resetParser
                <||> stepParser
                <||> quitParser
                <||> loadParser)
                <* eof

parseCommand :: String -> Maybe Command
parseCommand = parseMaybe commandParser