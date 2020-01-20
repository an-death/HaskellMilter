module Network.Milter.Protocol.ProtocolOptions where


import Data.Bits ((.|.))


data Protocol
  = Null
  | NoConnect
  | NoHelo
  | NoMailFrom
  | NoRcptTo
  | NoBody
  | NoHeaders
  | NoEOH
  | Protocol Int
  deriving (Show, Eq)

instance Enum Protocol where
  fromEnum Null         = 0x00
  fromEnum NoConnect    = 0x01
  fromEnum NoHelo       = 0x02
  fromEnum NoMailFrom   = 0x04
  fromEnum NoRcptTo     = 0x08
  fromEnum NoBody       = 0x10
  fromEnum NoHeaders    = 0x20
  fromEnum NoEOH        = 0x40
  fromEnum (Protocol a) = a
  toEnum 0x00 = Null
  toEnum 0x01 = NoConnect
  toEnum 0x02 = NoHelo
  toEnum 0x04 = NoMailFrom
  toEnum 0x08 = NoRcptTo
  toEnum 0x10 = NoBody
  toEnum 0x20 = NoHeaders
  toEnum 0x40 = NoEOH
  toEnum a    = Protocol a

instance Semigroup Protocol where
  (<>) a b = Protocol $ fromEnum a .|. fromEnum b

instance Monoid Protocol where
  mempty = Null
  mappend = (<>)
