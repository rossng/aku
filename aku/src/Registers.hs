{-# LANGUAGE TemplateHaskell #-}
module Registers where

import Data.Word
import Control.Lens
import Text.Printf

data RegisterName = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7
              deriving (Show, Eq, Ord)

data Registers = Registers {
      _x0 :: Word32
    , _x1 :: Word32
    , _x2 :: Word32
    , _x3 :: Word32
    , _x4 :: Word32
    , _x5 :: Word32
    , _x6 :: Word32
    , _x7 :: Word32
} deriving (Eq)

makeLenses ''Registers

instance Show Registers where
    show regs = "REGISTERS  X0       X1       X2       X3       X4       X5       X6       X7      " ++ "\n"
             ++ "           "
             ++ printf "%08x " (regs^.x0)
             ++ printf "%08x " (regs^.x1)
             ++ printf "%08x " (regs^.x2)
             ++ printf "%08x " (regs^.x3)
             ++ printf "%08x " (regs^.x4)
             ++ printf "%08x " (regs^.x5)
             ++ printf "%08x " (regs^.x6)
             ++ printf "%08x" (regs^.x7)

readRegister :: Registers -> RegisterName -> Word32
readRegister registers register = case register of
    X0 -> 0
    X1 -> registers^.x1
    X2 -> registers^.x2
    X3 -> registers^.x3
    X4 -> registers^.x4
    X5 -> registers^.x5
    X6 -> registers^.x6
    X7 -> registers^.x7

emptyRegisters :: Registers
emptyRegisters = Registers 0 0 0 0 0 0 0 0

writeRegister :: RegisterName -> Word32 -> Registers -> Registers
writeRegister register value registers = case register of
    X0 -> registers
    X1 -> registers & x1 .~ value
    X2 -> registers & x2 .~ value
    X3 -> registers & x3 .~ value
    X4 -> registers & x4 .~ value
    X5 -> registers & x5 .~ value
    X6 -> registers & x6 .~ value
    X7 -> registers & x7 .~ value