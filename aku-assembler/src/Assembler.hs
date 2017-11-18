{-# LANGUAGE OverloadedStrings #-}
module Assembler where

import Control.Monad (void)
import Data.Void
import Data.Text
import Data.Int
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Instruction
import Registers

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (() <$ some (char ' ')) lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

unsignedInteger :: Parser Word32
unsignedInteger = fromIntegral <$> integer

signedInteger :: Parser Int32
signedInteger = fromIntegral <$> L.signed sc integer

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = ["ADD","ADDI","NAND","LUI","SW","LW","BEQ","JALR","HALT"]

registerParser = try (X0 <$ rword "X0") <|>
                try (X1 <$ rword "X1") <|>
                try (X2 <$ rword "X2") <|>
                try (X3 <$ rword "X3") <|>
                try (X4 <$ rword "X4") <|>
                try (X5 <$ rword "X5") <|>
                try (X6 <$ rword "X6") <|>
                try (X7 <$ rword "X7")

destParser = Dest <$> registerParser
sourceParser = Source <$> registerParser
sImmParser = ImmS <$> signedInteger
uImmParser = ImmU <$> unsignedInteger

addParser = ADD <$ rword "ADD" <*> destParser <*> sourceParser <*> sourceParser
addiParser = ADDI <$ rword "ADDI" <*> destParser <*> sourceParser <*> sImmParser
nandParser = NAND <$ rword "NAND" <*> destParser <*> sourceParser <*> sourceParser
luiParser = LUI <$ rword "LUI" <*> destParser <*> uImmParser
swParser = SW <$ rword "SW" <*> sourceParser <*> sourceParser <*> sImmParser
lwParser = LW <$ rword "LW" <*> destParser <*> sourceParser <*> sImmParser
beqParser = BEQ <$ rword "BEQ" <*> sourceParser <*> sourceParser <*> sImmParser
jalrParser = JALR <$ rword "JALR" <*> destParser <*> sourceParser
haltParser = HALT <$ rword "HALT"

statementParser =     try addParser
                  <|> try addiParser
                  <|> try nandParser
                  <|> try luiParser
                  <|> try swParser
                  <|> try lwParser
                  <|> try beqParser
                  <|> try jalrParser
                  <|> try haltParser

programParser = sepEndBy1 statementParser eol <* eof

