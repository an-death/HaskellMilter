{-# LANGUAGE OverloadedStrings #-}

module Network.Milter.Protocol (
    Packet (..)
  , getPacket
  , getIP
  , getKeyVal
  , getBody
  , negotiate
  , accept, discard, hold, reject, continue
  , Action(..)
  , Protocol(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.IP
import Data.List (foldl')
import Data.Monoid
import System.IO

import Network.Milter.Packet
import Network.Milter.Actions

import Data.Bits (Bits(..))

----------------------------------------------------------------

accept :: Handle -> IO ()
accept hdl   = safePutPacket hdl $ Packet 'a' ""

discard :: Handle -> IO ()
discard hdl  = safePutPacket hdl $ Packet 'd' ""

hold :: Handle -> IO ()
hold hdl     = safePutPacket hdl $ Packet 't' ""

reject :: Handle -> IO ()
reject hdl   = safePutPacket hdl $ Packet 'r' ""

continue :: Handle -> IO ()
continue hdl = safePutPacket hdl $ Packet 'c' ""

----------------------------------------------------------------

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

negotiate :: Action -> Protocol -> Handle -> IO ()
negotiate action protocol hdl =  putPacket hdl negoPkt -- do NOT use safePutPacket
  where
    version = intToFourBytes 2 -- Sendmail 8.13.8, sigh
    act = intToFourBytes (fromEnum action)
    proto = intToFourBytes (fromEnum protocol)
    negoPkt :: Packet
    negoPkt = Packet 'O' $ toByteString $ version <> act <> proto


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


instance Bits Protocol where
  (.&.) a b = Protocol $ fromEnum a .&. fromEnum b
  (.|.) a b = Protocol $ fromEnum a .|. fromEnum b
  xor a b   = Protocol $ fromEnum a `xor` fromEnum b

----------------------------------------------------------------
