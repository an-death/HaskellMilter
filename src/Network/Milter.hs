module Network.Milter
  ( milter
  , MilterHandler(..)
  , defaultMilterHandler
  , Response(..)
  , MessageModificator
  ) where

import Network.Milter.Handler  (MilterHandler(..), defaultMilterHandler)
import Network.Milter.Modifier (MessageModificator)
import Network.Milter.Packet   (Response(..))
import Network.Milter.Switch   (milter)
