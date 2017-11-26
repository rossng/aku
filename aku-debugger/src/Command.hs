module Command where

import Data.Void
import Data.Char
import Data.Int
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Registers

type Parser = Parsec Void String

data Command =  Reset -- resets the CPU to initialCPU
              | LoadProgram String -- load a program into memory
              | Step -- step forward one clock cycle
              | StepN Int -- step forward n clock cycles
              | SetMemory Int [Int32] -- set a contiguous series of values in memory
              | SetRegister RegisterName Word32 -- set a register to a value
              | Continue -- continue until halt
              | ShowStall -- show the output of the stall logic
              | ShowStomp -- show the output of the stall logic
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

integer :: Parser Int
integer = lexeme L.decimal

signedInteger :: Parser Int
signedInteger = L.signed space integer

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

quoted :: Parser a -> Parser a
quoted = between (symbol "\"") (symbol "\"")

list :: Parser a -> Parser a
list = between (symbol "[") (symbol "]")

filePathParser :: Parser String
filePathParser = quoted (some (satisfy (\c -> not (isSpace c) && not (isControl c) && c /= '"')))

valueListParser :: Parser [Int32]
valueListParser = list $ map fromIntegral <$> sepBy1 signedInteger (symbol ",")

registerNameParser :: Parser RegisterName
registerNameParser =      X0 <$ try (rword "X0")
                     <|>  X1 <$ try (rword "X1")
                     <|>  X2 <$ try (rword "X2")
                     <|>  X3 <$ try (rword "X3")
                     <|>  X4 <$ try (rword "X4")
                     <|>  X5 <$ try (rword "X5")
                     <|>  X6 <$ try (rword "X6")
                     <|>  X7 <$ try (rword "X7")

resetParser :: Parser Command
resetParser =     Reset <$ try (rword "reset")
              <|> Reset <$ try (rword "r")

stepNParser :: Parser Command
stepNParser =    StepN <$ try (rword "step") <*> integer
             <|> StepN <$ try (rword "s") <*> integer

stepParser :: Parser Command
stepParser =     Step <$ try (rword "step")
             <|> Step <$ try (rword "s")

quitParser :: Parser Command
quitParser =      Quit <$ try (rword "quit")
              <|> Quit <$ try (rword "q")

loadParser :: Parser Command
loadParser =      LoadProgram <$ try (rword "load") <*> filePathParser
              <|> LoadProgram <$ try (rword "l") <*> filePathParser

setMemParser :: Parser Command
setMemParser = SetMemory <$ try (rword "setm") <*> integer <*> valueListParser

setRegParser :: Parser Command
setRegParser = SetRegister <$ try (rword "setr") <*> registerNameParser <*> (fromIntegral <$> signedInteger)

continueParser :: Parser Command
continueParser =     Continue <$ try (rword "continue")
                 <|> Continue <$ try (rword "c")

stallParser :: Parser Command
stallParser = ShowStall <$ try (rword "stall")

stompParser :: Parser Command
stompParser = ShowStomp <$ try (rword "stomp")

commandParser :: Parser Command
commandParser = (    resetParser
                <||> stepNParser
                <||> stepParser
                <||> quitParser
                <||> loadParser
                <||> setMemParser
                <||> setRegParser
                <||> continueParser
                <||> stallParser
                <||> stompParser
                ) <* eof

parseCommand :: String -> Maybe Command
parseCommand = parseMaybe commandParser