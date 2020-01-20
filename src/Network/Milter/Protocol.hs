{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Network.Milter.Protocol (
    Packet (..)
  , getPacket
                               , putPacket
  , getIP
  , getKeyVal
  , getBody
  , negotiate
  , accept, discard, hold, reject, continue
  , Action(..)
  , Protocol(Null , NoConnect , NoHelo , NoMailFrom , NoRcptTo , NoBody , NoHeaders ,
  NoEOH)
  , ResponsePacket
                               , newModificator
                               , safePutPacket
                               , MessageModificator
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.IP
import Data.List (foldl')
import Data.Monoid
import System.IO

import Network.Milter.Packet
import Network.Milter.Actions
import Network.Milter.Modifier

import Data.Bits ((.|.))

----------------------------------------------------------------

negotiate :: Action -> Protocol -> ResponsePacket
negotiate action protocol = negoPkt -- do NOT use safePutPacket
  where
    version = intToFourBytes 2 -- Sendmail 8.13.8, sigh
    act = intToFourBytes (fromEnum action)
    proto = intToFourBytes (fromEnum protocol)
    negoPkt :: Packet
    negoPkt = Packet 'O' $ toByteString $ version <> act <> proto


accept :: ResponsePacket
accept = Packet 'a' ""

discard :: ResponsePacket
discard = Packet 'd' ""

hold :: ResponsePacket
hold = Packet 't' ""

reject :: ResponsePacket
reject = Packet 'r' ""

continue :: ResponsePacket
continue = Packet 'c' ""

----------------------------------------------------------------

getKeyVal :: ByteString -> (ByteString, ByteString)
getKeyVal bs = (key,val)
  where
    kv = BS.split '\0' bs
    key = kv !! 0
    val = kv !! 1

----------------------------------------------------------------

getBody :: ByteString -> ByteString
getBody = BS.init -- removing the last '\0'

----------------------------------------------------------------

getIP :: ByteString -> IP
getIP bs
  | fam == '4' = IPv4 . read $ adr
  | otherwise  = IPv6 . read $ adr
  where
    ip  = BS.split '\0' bs !! 1
    fam = BS.head ip
    adr = BS.unpack $ BS.drop 3 ip

----------------------------------------------------------------


data Protocol = Null | NoConnect | NoHelo | NoMailFrom | NoRcptTo | NoBody | NoHeaders |
  NoEOH | Protocol Int
  deriving (Show, Eq)

instance Enum Protocol where
  fromEnum Null = 0x00
  fromEnum NoConnect  = 0x01
  fromEnum NoHelo = 0x02
  fromEnum NoMailFrom    = 0x04
  fromEnum NoRcptTo    = 0x08
  fromEnum NoBody  = 0x10
  fromEnum NoHeaders    = 0x20
  fromEnum NoEOH    = 0x40
  fromEnum (Protocol a) = a

  toEnum  0x00 = Null
  toEnum  0x01 = NoConnect
  toEnum  0x02 = NoHelo
  toEnum  0x04 = NoMailFrom
  toEnum  0x08 = NoRcptTo
  toEnum  0x10 = NoBody
  toEnum  0x20 = NoHeaders
  toEnum  0x40 = NoEOH
  toEnum  a    = Protocol a

instance Semigroup Protocol where 
        (<>) a b =  Protocol $ fromEnum a .|. fromEnum b
instance Monoid Protocol where
  mempty = Null
  mappend = (<>)
----------------------------------------------------------------
