{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception       (bracket)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Network.Milter          as Milter
    ( MessageModificator
    , MilterHandler(..)
    , ResponsePacket
    , defaultMilterHandler
    , milter
    )
import qualified Network.Milter.Protocol as Opt (Action(..), Protocol(..))

import qualified Network.Simple.TCP      as TCP (HostPreference(Host), serve)
import           Network.Socket          (socketToHandle)
import           System.IO
    (BufferMode(NoBuffering), IOMode(ReadWriteMode), hClose, hSetBuffering)

main :: IO ()
main =
  TCP.serve (TCP.Host "127.0.0.1") "9939" $ \(connectionSocket, remoteAddr) ->
    bracket
      (openHandle connectionSocket)
      closeHandle
      (\hdl -> do
         hSetBuffering hdl NoBuffering
         putStrLn $ "TCP connection established from " ++ show remoteAddr
         Milter.milter myMilter hdl
         putStrLn $ "milter Done " ++ show remoteAddr)

openHandle = flip socketToHandle ReadWriteMode

closeHandle = hClose

myMilter = Milter.defaultMilterHandler {Milter.open = open, Milter.eom = eom}

open :: (MonadIO m) => m (Opt.Action, Opt.Protocol)
open =
  liftIO $ do
    putStrLn "Milter opened from "
    let onlyConnect =
          Opt.NoHelo <>
          Opt.NoMailFrom <>
          Opt.NoRcptTo <> Opt.NoBody <> Opt.NoHeaders <> Opt.NoEOH
    return (Opt.NoAction, onlyConnect)

------------------------------------------------------------------
--conn hdl bs = do
--  print bs
--  continue hdl
--
------------------------------------------------------------------
--helo hdl _ = do
--  putStrLn "HELO"
--  continue hdl
--
------------------------------------------------------------------
--mfro hdl bs = do
--  putStrLn "MAIL FROM"
--  continue hdl
--
------------------------------------------------------------------
--hedr hdl bs = do
--  putStrLn "DATA HEADER FIELD"
--  continue hdl
--
------------------------------------------------------------------
--eohd hdl _ = do
--  putStrLn "DATA HEADER END"
--  continue hdl
--
------------------------------------------------------------------
--body hdl bs = do
--  putStrLn "DATA BODY"
--  continue hdl
--
--
------------------------------------------------------------------
eom :: (MonadIO m) => Milter.MessageModificator -> m Milter.ResponsePacket
eom _ =
  liftIO $ do
    putStrLn "DATA BODY END"
    putStrLn "accepted"
    Milter.eom Milter.defaultMilterHandler undefined
