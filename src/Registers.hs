module Registers where

import Data.Word

data RegisterName = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7
              deriving (Show, Eq)

data Registers = Registers {
      x0 :: Word32
    , x1 :: Word32
    , x2 :: Word32
    , x3 :: Word32
    , x4 :: Word32
    , x5 :: Word32
    , x6 :: Word32
    , x7 :: Word32
} deriving (Show, Eq)

index :: RegisterName -> Int
index X0 = 0
index X1 = 1
index X2 = 2
index X3 = 3
index X4 = 4
index X5 = 5
index X6 = 6
index X7 = 7

readRegister :: Registers -> RegisterName -> Word32
readRegister (Registers x0 x1 x2 x3 x4 x5 x6 x7) register = case register of
    X0 -> 0
    X1 -> x1
    X2 -> x2
    X3 -> x3
    X4 -> x4
    X5 -> x5
    X6 -> x6
    X7 -> x7

emptyRegisters :: Registers
emptyRegisters = Registers 0 0 0 0 0 0 0 0

writeRegister :: Registers -> RegisterName -> Word32 -> Registers
writeRegister registers register value = case register of
    X0 -> registers
    X1 -> registers { x1 = value }
    X2 -> registers { x2 = value }
    X3 -> registers { x3 = value }
    X4 -> registers { x4 = value }
    X5 -> registers { x5 = value }
    X6 -> registers { x6 = value }
    X7 -> registers { x7 = value }