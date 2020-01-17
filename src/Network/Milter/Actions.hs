module Network.Milter.Actions
 (Action(..))where

import Data.Bits (Bits(..))

data Action = NoAction | AddHeader | ChangeBody | AddRcpt | RemoveRcpt|
  ChangeHeader | Quarantine | Action Int
  deriving (Show, Eq)


bits :: Action -> Int
bits NoAction = 0x00
bits AddHeader  = 0x01
bits ChangeBody = 0x02
bits AddRcpt    = 0x04
bits RemoveRcpt    = 0x08
bits ChangeHeader  = 0x10
bits Quarantine    = 0x20
bits (Action a) = a

instance Enum Action where
  fromEnum = bits
  toEnum  0x00 = NoAction
  toEnum  0x01 = AddHeader
  toEnum  0x02 = ChangeBody
  toEnum  0x04 = AddRcpt
  toEnum  0x08 = RemoveRcpt
  toEnum  0x10 = ChangeHeader
  toEnum  0x20 = Quarantine
  toEnum  a    = Action a


instance Bits Action where
  (.&.) a b = Action $ fromEnum a .&. fromEnum b
  (.|.) a b = Action $ fromEnum a .|. fromEnum b
  xor a b   = Action $ fromEnum a `xor` fromEnum b
