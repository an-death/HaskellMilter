{-# LANGUAGE RankNTypes #-}

module Network.Milter.Handler where

import Control.Monad.IO.Class  (MonadIO)
import Data.ByteString         (ByteString)
import Network.Milter.Modifier (MessageModificator)
import Network.Milter.Protocol
    (Action(NoAction), Protocol(Null), ResponsePacket, accept, continue)

type Content = ByteString

type HandleFilterF
   = forall m. (MonadIO m) =>
                 Content -> MessageModificator -> m ResponsePacket

data MilterHandler =
  MilterHandler
    { open :: forall m. (MonadIO m) =>
                          m (Action, Protocol) -- negotiate Packet 'O'
    , connection :: HandleFilterF
    , helo :: HandleFilterF
    , mailFrom :: HandleFilterF
    , header :: HandleFilterF
    , eoheaders :: HandleFilterF
    , body :: HandleFilterF
    , eom :: forall m. (MonadIO m) =>
                         MessageModificator -> m (ResponsePacket)
    , abort :: forall m. (MonadIO m) =>
                           m ()
    }

defaultMilterHandler =
  MilterHandler
    { open = return (NoAction, Null)
    , connection = const2 (return continue)
    , helo = const2 (return continue)
    , mailFrom = const2 (return continue)
    , header = const2 (return continue)
    , eoheaders = const2 (return continue)
    , body = const2 (return continue)
    , eom = const (return accept)
    , abort = return ()
    }

const2 = const . const
