{-# LANGUAGE RankNTypes #-}

module Network.Milter.Handler where

import Control.Monad.IO.Class  (MonadIO)
import Data.ByteString         (ByteString)
import Network.Milter.Modifier (MessageModificator)
import Network.Milter.Protocol (Action(NoAction), Protocol(Null), Response(..))

type Content = ByteString

type HandleF
   = forall m. (MonadIO m) =>
                 Content -> MessageModificator -> m Response

data MilterHandler =
  MilterHandler
    { open :: forall m. (MonadIO m) => m Response  -- negotiate Packet 'O'
    , connection :: HandleF
    , helo :: HandleF
    , mailFrom :: HandleF
    , header :: HandleF
    , eoheaders :: HandleF
    , body :: HandleF
    , eom :: forall m. (MonadIO m) => MessageModificator -> m Response
    , abort :: forall m. (MonadIO m) => m ()
    }

defaultMilterHandler :: MilterHandler
defaultMilterHandler =
  MilterHandler
    { open = return (Negotiate 2 NoAction Null)
    , connection = const2 (return Continue)
    , helo = const2 (return Continue)
    , mailFrom = const2 (return Continue)
    , header = const2 (return Continue)
    , eoheaders = const2 (return Continue)
    , body = const2 (return Continue)
    , eom = const (return Accept)
    , abort = return ()
    }

const2 :: a -> b -> c -> a
const2 = const . const
