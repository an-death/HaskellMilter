{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Network.Milter.Protocol
  ( Packet(..)
  , getPacket
  , putPacket
  , getIP
  , getKeyVal
  , getBody
  , Action(..)
  , Protocol(Null, NoConnect, NoHelo, NoMailFrom, NoRcptTo, NoBody,
         NoHeaders, NoEOH)
  , Response(..)
  , asStdPacket
  , newModificator
  , safePutPacket
  , MessageModificator
  ) where

import           Data.ByteString.Char8                   (ByteString)
import qualified Data.ByteString.Char8                   as BS
import           Data.IP

import           Network.Milter.Modifier
import           Network.Milter.Packet
import           Network.Milter.Protocol.Actions
import           Network.Milter.Protocol.ProtocolOptions

getKeyVal :: ByteString -> (ByteString, ByteString)
getKeyVal bs = (key, val)
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
  | otherwise = IPv6 . read $ adr
  where
    ip = BS.split '\0' bs !! 1
    fam = BS.head ip
    adr = BS.unpack $ BS.drop 3 ip

----------------------------------------------------------------
----------------------------------------------------------------
