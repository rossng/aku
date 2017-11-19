{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Monad (void)
import Data.Void
import Data.Text
import Data.Int
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Instruction as I
import Registers

type Parser = Parsec Void String

data Statement =
      ADD I.DestRegister I.SourceRegister I.SourceRegister
    | ADDI I.DestRegister I.SourceRegister I.SignedImmediate
    | NAND I.DestRegister I.SourceRegister I.SourceRegister
    | LUI I.DestRegister I.UnsignedImmediate
    | SW I.SourceRegister I.SourceRegister I.SignedImmediate
    | LW I.DestRegister I.SourceRegister I.SignedImmediate
    | BEQI I.SourceRegister I.SourceRegister I.SignedImmediate
    | BEQL I.SourceRegister I.SourceRegister String
    | BLTI I.SourceRegister I.SourceRegister I.SignedImmediate
    | BLTL I.SourceRegister I.SourceRegister String
    | JALR I.DestRegister I.SourceRegister
    | HALT
    | LABEL String
    deriving (Show, Eq)

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
unsignedInteger = fromIntegral <$> integer <?> "unsigned integer"

signedInteger :: Parser Int32
signedInteger = fromIntegral <$> L.signed sc integer <?> "signed integer"

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = some alphaNumChar <?> "identifier"

registerParser =    try (X0 <$ rword "X0")
                <|> try (X1 <$ rword "X1")
                <|> try (X2 <$ rword "X2")
                <|> try (X3 <$ rword "X3")
                <|> try (X4 <$ rword "X4")
                <|> try (X5 <$ rword "X5")
                <|> try (X6 <$ rword "X6")
                <|> try (X7 <$ rword "X7")
                <?> "register"

destParser      = I.Dest <$> registerParser
sourceParser    = I.Source <$> registerParser
sImmParser      = I.ImmS <$> signedInteger
uImmParser      = I.ImmU <$> unsignedInteger

addParser   = ADD <$ rword "ADD" <*> destParser <*> sourceParser <*> sourceParser
addiParser  = ADDI <$ rword "ADDI" <*> destParser <*> sourceParser <*> sImmParser
nandParser  = NAND <$ rword "NAND" <*> destParser <*> sourceParser <*> sourceParser
luiParser   = LUI <$ rword "LUI" <*> destParser <*> uImmParser
swParser    = SW <$ rword "SW" <*> sourceParser <*> sourceParser <*> sImmParser
lwParser    = LW <$ rword "LW" <*> destParser <*> sourceParser <*> sImmParser
beqiParser  = BEQI <$ rword "BEQ" <*> sourceParser <*> sourceParser <*> sImmParser
beqlParser  = BEQL <$ rword "BEQ" <*> sourceParser <*> sourceParser <*> identifier
bltiParser  = BLTI <$ rword "BLT" <*> sourceParser <*> sourceParser <*> sImmParser
bltlParser  = BLTL <$ rword "BLT" <*> sourceParser <*> sourceParser <*> identifier
jalrParser  = JALR <$ rword "JALR" <*> destParser <*> sourceParser
haltParser  = HALT <$ rword "HALT"
labelParser = LABEL <$> lexeme identifier <* symbol ":"

statementParser :: Parser Statement
statementParser =     try addParser
                  <|> try addiParser
                  <|> try nandParser
                  <|> try luiParser
                  <|> try swParser
                  <|> try lwParser
                  <|> try beqiParser
                  <|> try beqlParser
                  <|> try bltiParser
                  <|> try bltlParser
                  <|> try jalrParser
                  <|> try haltParser
                  <|> try labelParser

programParser :: Parser [Statement]
programParser = sepEndBy1 statementParser eol <* eof

