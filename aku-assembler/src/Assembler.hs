{-# LANGUAGE OverloadedLists #-}
module Assembler where

import Parser
import Data.Int
import Data.List
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Control.Monad.State
import qualified Instruction as I
import qualified Memory as M
import Text.Megaparsec

data LabelState = PS Int [(String, Int)] deriving (Eq, Show)

stmtToInstruction :: Int -> [(String, Int)] -> Statement -> Either String [I.Instruction]
stmtToInstruction addr labels stmt =
    case stmt of
        ADD d s1 s2     -> Right [I.ADD d s1 s2]
        ADDI d s i      -> Right [I.ADDI d s i]
        NAND d s1 s2    -> Right [I.NAND d s1 s2]
        LUI d u         -> Right [I.LUI d u]
        SW s1 s2 i      -> Right [I.SW s1 s2 i]
        LW d s i        -> Right [I.LW d s i]
        BEQI s1 s2 i    -> Right [I.BEQ s1 s2 i]
        BEQL s1 s2 l    -> case do relativeAddr <- getRelativeAddr l
                                   return (I.BEQ s1 s2 relativeAddr) of
                            Just i -> Right [i]
                            Nothing -> Left $ "Label " ++ l ++ " not defined"
        BLTI s1 s2 i    -> Right [I.BLT s1 s2 i]
        BLTL s1 s2 l    -> case do relativeAddr <- getRelativeAddr l
                                   return (I.BLT s1 s2 relativeAddr) of
                            Just i -> Right [i]
                            Nothing -> Left $ "Label " ++ l ++ " not defined"
        JALR d s        -> Right [I.JALR d s]
        HALT            -> Right [I.HALT]
        LABEL _         -> Right []
  where getRelativeAddr l = do  label <- find (\e -> fst e == l) labels
                                let labelAddr = snd label :: Int
                                return (I.ImmS $ fromIntegral (labelAddr - addr - 2))

replaceLabels :: [Statement] -> Either String [I.Instruction]
replaceLabels [] = Right []
replaceLabels stmts@(s:ss) = replaceLabels' labels stmts 0
    where labels = process stmts

replaceLabels' :: [(String, Int)] -> [Statement] -> Int -> Either String [I.Instruction]
replaceLabels' _ [] _ = Right []
replaceLabels' labels (s:ss) addr = do
                                instructions <- stmtToInstruction addr labels s
                                restInstructions <- replaceLabels' labels ss (addr + length instructions)
                                return (instructions ++ restInstructions)

processStatement :: LabelState -> Statement -> LabelState
processStatement (PS addr labels) (LABEL s) = PS addr ((s, addr + 1) : labels)
processStatement (PS addr labels) _ = PS (addr + 1) labels

process :: [Statement] -> [(String, Int)]
process ss = labels
    where PS _ labels = foldl processStatement (PS 0 []) ss

parseFromFile :: Parsec e String a -> String -> IO (Either (ParseError (Token String) e) a)
parseFromFile p file = runParser p file <$> readFile file

loadProgram :: FilePath -> IO (Maybe M.Program)
loadProgram f = do stmts <- parseFromFile programParser f
                   case stmts of
                    Left _      -> return Nothing
                    Right ss    -> case replaceLabels ss of
                        Left _      -> return Nothing
                        Right is    -> return $ Just (M.Program (V.fromList is))