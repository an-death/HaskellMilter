{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.Simple.TCP as TCP (serve, HostPreference(Host))
import Network.Socket (socketToHandle, sIsConnected)
import System.IO (IOMode(ReadWriteMode), hSetBuffering, BufferMode(NoBuffering), hClose, hIsClosed)
import Data.ByteString (hPut)
import Control.Exception (bracket)
import Network.Milter (milter)

main :: IO ()
main = TCP.serve (TCP.Host "127.0.0.1") "9939" $ \(connectionSocket, remoteAddr) ->
    bracket (openHandle connectionSocket) closeHandle
      (\hdl -> do
        hSetBuffering hdl NoBuffering
        putStrLn $ "TCP connection established from " ++ show remoteAddr
        milter hdl
        putStrLn $ "milter Done " ++ show remoteAddr
        )

openHandle = flip socketToHandle $ ReadWriteMode
closeHandle = hClose





