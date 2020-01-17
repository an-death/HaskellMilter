{-# LANGUAGE OverloadedStrings #-}

module Network.Milter.Switch where

import Control.Exception (SomeException(..), handle)
import Control.Monad (unless)
import Data.ByteString.Char8 (ByteString, unpack)
import System.IO (Handle, hClose, hIsClosed, hIsEOF)

import Network.Milter.Protocol
  ( Action(NoAction)
  , Packet(..)
  , Protocol(..)
  , accept
  , continue
  , discard
  , getPacket
  , hold
  , negotiate
  , reject
  )

import Network.Milter.Handler (MilterHandler(..))


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
switch mltr hdl (Packet 'O' _) = open mltr hdl
switch mltr hdl (Packet 'C' bs) = connection mltr bs hdl
switch mltr hdl (Packet 'H' bs) = helo mltr bs hdl
switch mltr hdl (Packet 'M' bs) = mailFrom mltr bs hdl
switch mltr hdl (Packet 'R' _) = continue hdl
switch mltr hdl (Packet 'A' bs) = abort mltr
switch mltr hdl (Packet 'L' bs) = header mltr bs hdl
switch mltr hdl (Packet 'N' bs) = eoheaders mltr bs hdl
switch mltr hdl (Packet 'B' bs) = body mltr bs hdl
switch mltr hdl (Packet 'E' _) = eom mltr hdl
switch _ _ (Packet 'D' _) = return ()
switch _ hdl (Packet 'Q' _) = hClose hdl
switch _ hdl (Packet 'T' _) = continue hdl --- T command ¯\_(ツ)_/¯
switch _ _ (Packet x _) = putStrLn $ "Switch: \"" ++ [x]
