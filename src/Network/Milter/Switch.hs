{-# LANGUAGE OverloadedStrings #-}

module Network.Milter.Switch where


import System.IO (Handle, hIsClosed, hIsEOF)
import Control.Exception (SomeException(..), handle)
import Control.Monad (unless)
import Data.ByteString.Char8 (ByteString, unpack)

import Network.Milter.Protocol (Packet(..), getPacket, accept,discard,continue,hold,
  reject, negotiate)
import Network.Milter.Protocol (Action(NoAction), Protocol(..))
import Data.Bits ((.|.))

milter :: Handle -> IO ()
milter hdl  = withValidHandleDo $
    handle errorHandle $ do
      rpkt <- getPacket hdl
      switch hdl rpkt
      milter hdl
  where
--    errorHandle (SomeException e) = logDebug env ref $ show e
    errorHandle (SomeException e) = putStrLn $ "ERR: " ++ show e
    withValidHandleDo blk = do
        closed <- hIsClosed hdl
        eof <- hIsEOF hdl
        unless (eof || closed) blk


switch :: Handle -> Packet -> IO ()
switch hdl (Packet 'O' bs) = open hdl bs
switch hdl (Packet 'C' bs) = conn hdl bs
switch hdl (Packet 'H' bs) = helo hdl bs
switch hdl (Packet 'M' bs) = mfro hdl bs
switch hdl (Packet 'R' _ ) = continue hdl
switch hdl (Packet 'A' _ ) = continue hdl -- xxx
switch hdl (Packet 'L' bs) = hedr hdl bs
switch hdl (Packet 'N' bs) = eohd hdl bs
switch hdl (Packet 'B' bs) = body hdl bs
switch hdl (Packet 'E' bs) = eoms hdl bs
switch _   (Packet 'D'  _) = return ()
switch _   (Packet 'Q'  _) = return ()
switch hdl (Packet 'T'  _) = continue hdl  --- T command ¯\_(ツ)_/¯
switch _   (Packet x    bs) = putStrLn $ "Switch: \"" ++ [x] ++ "\" data: " ++  unpack bs


----------------------------------------------------------------

type Filter = Handle -> ByteString -> IO ()

----------------------------------------------------------------

open :: Filter
open hdl _ = do
    putStrLn "Milter opened"
    let onlyConnect = NoHelo .|. NoMailFrom .|. NoRcptTo .|. NoBody .|. NoHeaders .|. NoEOH
    negotiate NoAction onlyConnect hdl

----------------------------------------------------------------

conn :: Filter
conn hdl bs = do
     print bs
     continue hdl
--    st <- readIORef ref
--    let ip = getIP bs
--        ms = (mailspec st) { msPeerIP = ip }
--    writeIORef ref st { mailspec = ms }
--    -- after IP set for logging
--    logResult env ref "SMTP connected"
--    logDebug env ref $ '\t' : show ms
--    mfilter env hdl ref ms B_Connect

----------------------------------------------------------------

helo :: Filter
helo hdl _ = do
  putStrLn "HELO"
  continue hdl

----------------------------------------------------------------

mfro :: Filter
mfro hdl bs = do
    putStrLn "MAIL FROM"
    continue hdl
--    let jmailfrom = extractDomain bs
--    case jmailfrom of
--      Nothing -> continue hdl -- xxx
--      Just dom  -> do
--          st <- readIORef ref
--          xspf <- getSPF dom st
--          let ms = (mailspec st) { msSPFResult = xspf, msMailFrom = jmailfrom }
--          writeIORef ref st { mailspec = ms }
--          logDebug env ref $ '\t' : show ms
--          mfilter env hdl ref ms B_MailFrom
--  where
--    getSPF dom st = do
--        let ip = msPeerIP (mailspec st)
--        spf env dom ip

----------------------------------------------------------------

hedr :: Filter
hedr hdl bs = do
    putStrLn "DATA HEADER FIELD"
    continue hdl
--    st <- readIORef ref
--    let (key,val) = getKeyVal bs
--        ckey = canonicalizeKey key
--        xm = pushField key val (xmail st)
--        prd = pushPRD key val (prdspec st)
--        (pv,ms) = checkField ckey val (parsedv st) (mailspec st)
--    writeIORef ref $ st { xmail = xm, mailspec = ms, prdspec = prd, parsedv = pv}
--    logDebug env ref $ "\t" ++ show xm ++ "\n\t" ++ show ms ++ "\n\t" ++ show prd
--    continue hdl
--  where
--    checkField ckey val pv ms
--      | ckey == dkFieldKey   = case parseDK val of
--          Nothing -> (pv, ms)
--          x@(Just pdk) -> (pv { mpdk = x},
--                           ms { msSigDK = True
--                              , msDKFrom = Just (dkDomain pdk) })
--      | ckey == dkimFieldKey = case parseDKIM val of
--          Nothing -> (pv, ms)
--          x@(Just pdkim) -> (pv { mpdkim = x},
--                             ms { msSigDKIM = True
--                                , msDKIMFrom = Just (dkimDomain pdkim) })
--      | otherwise = (pv, ms)

----------------------------------------------------------------

eohd :: Filter
eohd hdl _ = do
    putStrLn "DATA HEADER END"
    continue hdl
--    st <- readIORef ref
--    let jfrom = getFrom st
--        jprd  = decidePRD (prdspec st)
--    sid <- getSenderID jprd
--    let ms = (mailspec st) { msSenderIDResult = sid, msFrom = jfrom, msPRA = jprd }
--    writeIORef ref st { mailspec = ms }
--    mfilter env hdl ref ms B_Header
--  where
--    getFrom = decideFrom . prdspec
--    getSenderID jprd =
--        case jprd of
--          Nothing  -> return DAPermError
--          Just dom -> do
--              ms <- mailspec <$> readIORef ref
--              if jprd == msMailFrom ms
--                 then return $ msSPFResult ms
--                 else let ip = msPeerIP ms
--                      in spf env dom ip

----------------------------------------------------------------

body :: Filter
body hdl bs = do
    putStrLn "DATA BODY"
    continue hdl
--    st <- readIORef ref
--    let bc = getBody bs
--        xm = pushBody bc (xmail st)
--    writeIORef ref st { xmail = xm }
--    continue hdl

----------------------------------------------------------------

eoms :: Filter
eoms hdl _ = do
  putStrLn "DATA BODY END"
  accept hdl
  putStrLn "accepted"
--  st <- readIORef ref
--  let mail = finalizeMail (xmail st)
--      mdk = mpdk (parsedv st)
--      mdkim = mpdkim (parsedv st)
--  xdk <- maybe (return DANone) (dk env mail) mdk
--  xdkim <- maybe (return DANone) (dkim env mail) mdkim
--  let ms = (mailspec st) { msDKResult = xdk, msDKIMResult = xdkim }
--  mfilter env hdl ref ms B_Body

