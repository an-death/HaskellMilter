{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Network.Milter.Modifier
  ( EmailModificator(addRecipient, deleteRecipient, replaceBody,
                 addHeader, changeHeader, quarantine)
  , newModificator
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Binary.Put (putInt32be, runPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)

import Network.Milter.Packet (Packet(..))

data EmailModificator =
  EmailModificator
    { addRecipient :: forall m. (MonadIO m) =>
                                  ByteString -> m ()
    , deleteRecipient :: forall m. (MonadIO m) =>
                                     ByteString -> m ()
    , replaceBody :: forall m. (MonadIO m) =>
                                 ByteString -> m ()
    , addHeader :: forall m. (MonadIO m) =>
                               (ByteString, ByteString) -> m ()
    , changeHeader :: forall m. (MonadIO m) =>
                                  Int -> (ByteString, ByteString) -> m ()
    , quarantine :: forall m. (MonadIO m) =>
                                (ByteString) -> m ()
    }

type PutPacket =forall m. (MonadIO m) => (Packet -> m ())

newModificator :: PutPacket -> EmailModificator
newModificator putPacket =
  EmailModificator
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
    ((toStrict . runPut) (putInt32be (fromIntegral index)) <> name <> "\0" <> value <> "\0")

_quarantine reason = Packet 'q' (reason <> "\b")
