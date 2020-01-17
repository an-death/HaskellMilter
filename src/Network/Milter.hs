module Network.Milter (
  milter
, MilterHandler(..)
, defaultMilterHandler
) where

import Network.Milter.Switch (milter)
import Network.Milter.Handler (MilterHandler(..), defaultMilterHandler)