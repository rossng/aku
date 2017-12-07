{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module CPU where

import qualified Data.Vector as V
import Data.Word
import Data.Int
import Control.Lens
import Data.Bits
import Data.Maybe
import Control.Monad.Writer
import qualified Data.Map.Strict as Map

import Registers
import Instruction
import qualified Memory as M
import ReservationStation
import RegisterStatusTable
import Stats

initialRST :: RST
initialRST = Map.fromList [
      (X0, Nothing)
    , (X1, Nothing)
    , (X2, Nothing)
    , (X3, Nothing)
    , (X4, Nothing)
    , (X5, Nothing)
    , (X6, Nothing)
    , (X7, Nothing)
    ]

makeRSVs :: RSVType -> Int -> [RSV]
makeRSVs t n = map (\i -> RSV (RSVId t i) Nothing) [1..n]

initialLoadStoreQueue :: [RSV]
initialLoadStoreQueue = makeRSVs LoadStore 4

initialRSVs :: [RSV]
initialRSVs = makeRSVs QuickInt 2 ++ makeRSVs SlowInt 2 ++ makeRSVs Branch 1

data CPU = CPU {
      _program :: M.Program
    , _memory :: M.Memory
    , _registers :: Registers
    , _halted :: Bool
    , _pc :: Int
    , _rst :: RST
    , _loadStoreQueue :: [RSV]
    , _rsv :: [RSV]
} deriving (Eq)

makeLenses ''CPU

instance Show CPU where
    show cpu = show (cpu^.program) ++ "\n" ++
               show (cpu^.memory) ++ "\n" ++
               show (cpu^.registers) ++ "\n" ++
               (if cpu^.halted then "HALTED" else "ACTIVE") ++ "\n" ++
               show (cpu^.pc) ++ "\n" ++
               show (cpu^.rst) ++ "\n" ++
               show (cpu^.loadStoreQueue) ++ "\n" ++
               show (cpu^.rsv)

initialCPU :: CPU
initialCPU = CPU M.emptyProgram M.emptyMemory emptyRegisters False 0 initialRST initialLoadStoreQueue initialRSVs

update :: CPU -> Writer Stats CPU
update cpu = writer (cpu', Stats 1)
  where
    cpu' = if cpu^.halted then cpu else cpu

opToRSVType :: Opcode -> RSVType
opToRSVType OPADD = QuickInt
opToRSVType OPADDI = QuickInt
opToRSVType OPNAND = QuickInt
opToRSVType OPSW = LoadStore
opToRSVType OPLW = LoadStore
opToRSVType OPBEQ = Branch
opToRSVType OPBLT = Branch
opToRSVType OPJALR = Branch
opToRSVType OPHALT = Branch

dispatch :: CPU -> CPU
dispatch cpu = case rsvType of
    QuickInt    -> dispatchInt insn cpu
    LoadStore   -> dispatchLoadStore insn cpu
    Branch      -> dispatchBranch insn cpu
    where insn = M.getInstruction (cpu^.program) (cpu^.pc)
          op = insToOp insn
          rsvType = opToRSVType op

dispatchInt :: Instruction -> CPU -> CPU
dispatchInt = undefined

dispatchLoadStore :: Instruction -> CPU -> CPU
dispatchLoadStore = undefined

dispatchBranch :: Instruction -> CPU -> CPU
dispatchBranch = undefined