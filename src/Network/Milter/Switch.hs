{-# LANGUAGE OverloadedStrings #-}

module Network.Milter.Switch where

import Control.Exception       (SomeException(..), handle)
import Control.Monad           (unless)
import System.IO               (Handle, hClose, hIsClosed, hIsEOF)

import Network.Milter.Protocol
    ( Packet(..)
    , Response(..)
    , MessageModificator
    , asStdPacket
    , getPacket
    , newModificator
    , putPacket
    , safePutPacket
    )

import Network.Milter.Handler  (MilterHandler(..))

milter :: MilterHandler -> Handle -> IO ()
milter mltr hdl =
  withValidHandleDo $
  handle errorHandle $ do
    rpkt <- getPacket hdl
    switch mltr hdl rpkt
    milter mltr hdl
  where
    errorHandle (SomeException e) = putStrLn $ "ERR: " ++ show e
    withValidHandleDo blk = do
      closed <- hIsClosed hdl
      eof <- hIsEOF hdl
      unless (eof || closed) blk

--    errorHandle (SomeException e) = logDebug env ref $ show e
switch :: MilterHandler -> Handle -> Packet -> IO ()
switch mltr hdl (Packet 'O' _) = open mltr >>= putPacket hdl . asStdPacket
switch mltr hdl (Packet 'C' bs) = connection mltr bs (modifier hdl) >>= response hdl
switch mltr hdl (Packet 'H' bs) =  helo mltr bs (modifier hdl) >>= response hdl
switch mltr hdl (Packet 'M' bs) =  mailFrom mltr bs (modifier hdl) >>= response hdl
switch _ hdl (Packet 'R' _) = response hdl Continue
switch mltr _ (Packet 'A' _) = abort mltr
switch mltr hdl (Packet 'L' bs) =  header mltr bs (modifier hdl) >>= response hdl
switch mltr hdl (Packet 'N' bs) =  eoheaders mltr bs (modifier hdl) >>= response hdl
switch mltr hdl (Packet 'B' bs) =  body mltr bs (modifier hdl) >>= response hdl
switch mltr hdl (Packet 'E' _) = eom mltr (modifier hdl) >>= response hdl
switch _ _ (Packet 'D' _) = return ()
switch _ hdl (Packet 'Q' _) = hClose hdl
switch _ hdl (Packet 'T' _) = response hdl Continue --- T command ¯\_(ツ)_/¯
switch _ _ (Packet x _) = putStrLn $ "Unknow command to switch: \"" ++ [x]

modifier :: Handle -> MessageModificator
modifier hdl = newModificator (safePutPacket hdl)

response :: Handle -> Response -> IO ()
response hdl = safePutPacket hdl . asStdPacket
