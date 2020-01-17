module Network.Milter.Handler where

import Data.ByteString (ByteString)

import System.IO (Handle)
import Network.Milter.Protocol (continue, accept)


type HandleFilterF = ByteString -> (Handle -> IO())

data MilterHandler = MilterHandler {
    open:: Handle -> IO()
  , connection:: HandleFilterF
  , helo :: HandleFilterF
  , mailFrom:: HandleFilterF
  , header :: HandleFilterF
  , eoheaders:: HandleFilterF
  , body :: HandleFilterF
  , eom :: Handle -> IO()
  , abort :: IO()
}

defaultMilterHandler = MilterHandler {
    open = continue
  , connection = const continue
  , helo = const continue
  , mailFrom = const continue
  , header = const continue
  , eoheaders = const continue
  , body = const continue
  , eom = accept
  , abort = return ()
}
