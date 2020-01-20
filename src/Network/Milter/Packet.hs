module Network.Milter.Packet where

import qualified Data.Binary.Builder as BinBuilder (putInt32be)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Builder (Builder, charUtf8, byteString, toLazyByteString)
import Data.List (foldl')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as X (unpack)

import System.IO (Handle, hIsClosed)
import Control.Monad (unless)
--
----------------------------------------------------------------

type ResponsePacket = Packet

data Packet = Packet Char ByteString

getPacket :: Handle -> IO Packet
getPacket hdl = do
    n <- fourBytesToInt <$> getNByte hdl 4
    Packet <$> getCmd hdl <*> getNByte hdl (n - 1)

putPacket :: Handle -> Packet -> IO ()
putPacket hdl (Packet c bs) = do
    let len = BS.length bs + 1
        pkt = intToFourBytes len <> charUtf8 c <> byteString bs
    BS.hPut hdl $ toByteString pkt

safePutPacket :: Handle -> Packet -> IO ()
safePutPacket hdl pkt = withOpenedHandleDo hdl $ putPacket hdl pkt

withOpenedHandleDo :: Handle -> IO () -> IO ()
withOpenedHandleDo hdl block = do
  closed <- hIsClosed hdl
  unless closed block


getNByte :: Handle -> Int -> IO ByteString
getNByte = BS.hGet

getCmd :: Handle -> IO Char
getCmd hdl = BSC.head <$> BS.hGet hdl 1

fourBytesToInt :: ByteString -> Int
fourBytesToInt = foldl' (\a b -> a * 256 + b) 0 . map fromIntegral . X.unpack

intToFourBytes :: Int -> Builder
intToFourBytes = BinBuilder.putInt32be . fromIntegral

toByteString = BSL.toStrict . toLazyByteString
