{-# LANGUAGE OverloadedStrings #-}

module Network.Milter.Modifier
  ( MessageModificator(addRecipient, deleteRecipient, replaceBody,
                   addHeader, changeHeader, quarantine)
  , newModificator
  ) where

import Data.Binary.Put       (putInt32be, runPut)
import Data.ByteString       (ByteString)
import Data.ByteString.Lazy  (toStrict)

import Network.Milter.Packet (Packet(..))

data MessageModificator =
  MessageModificator
    { addRecipient    :: ByteString -> IO ()
    , deleteRecipient :: ByteString -> IO ()
    , replaceBody     :: ByteString -> IO ()
    , addHeader       :: (ByteString, ByteString) -> IO ()
    , changeHeader    :: Int -> (ByteString, ByteString) -> IO ()
    , quarantine      :: (ByteString) -> IO ()
    }

type PutPacket = (Packet -> IO ())

newModificator :: PutPacket -> MessageModificator
newModificator putPacket =
  MessageModificator
    { addRecipient = putPacket . _addRecipient
    , deleteRecipient = putPacket . _deleteRecipient
    , replaceBody = putPacket . _replaceBody
    , addHeader = putPacket . _addHeader
    , changeHeader = \index -> putPacket . _changeHeader index
    , quarantine = putPacket . _quarantine
    }

recipientModificationPacket m recipient =
  Packet m ("<" <> recipient <> ">" <> "\0")

_addRecipient :: ByteString -> Packet
_addRecipient = recipientModificationPacket '+'

_deleteRecipient = recipientModificationPacket '-'

_replaceBody newBody = Packet 'b' (newBody <> "\0")

_addHeader (name, value) = Packet 'h' (name <> "\0" <> value <> "\0")

_changeHeader index (name, value) =
  Packet
    'm'
    ((toStrict . runPut) (putInt32be (fromIntegral index)) <>
     name <> "\0" <> value <> "\0")

_quarantine reason = Packet 'q' (reason <> "\b")
