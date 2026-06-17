{-# LANGUAGE RecordWildCards #-}

module ADHD.DHCP.Raw.Encoder where

import ADHD.DHCP.Types
import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Net.IPv4

-- | Putter for magic DHCP cookie
putCookie :: Put
putCookie = putWord32be 0x63825363

-- | Putter for raw option
putOption' :: RawOption -> Put
putOption' (Option t v) = do
  putWord8 t
  putWord8 $ fromIntegral $ BS.length v
  putByteString v
putOption' End = putWord8 255
putOption' Pad = putWord8 0

-- | Putter for unpacked raw option
putOption :: Word8 -> ByteString -> Put
putOption b = putOption' . Option b

-- | Convert CIDR to mask IP
maskToIp :: Int -> IPv4
maskToIp n = IPv4 $ 0xffffffff `shiftL` (32 - n)

-- | Putter for IPv4
putIP :: IPv4 -> Put
putIP = putWord32be . getIPv4

-- | Putter for raw messages
putMessage :: RawMessage -> Put
putMessage RawMessage {..} = do
  putWord8 2
  putWord8 htype
  putWord8 hlen
  putWord8 0
  putByteString xid
  putByteString secs
  putByteString flags

  putIP ciaddr
  putIP yiaddr
  putIP siaddr
  putIP giaddr

  putByteString chaddr
  putByteString $ BS.replicate 64 0
  putByteString $ BS.replicate 128 0

  putCookie
  putOptions options

-- | Putter for raw options
putOptions :: [RawOption] -> Put
putOptions ops = do
  let serializedOps = BS.toStrict $ runPut (mapM_ putOption' ops)
      totalLength = 4 + BS.length serializedOps
      remainder = totalLength `mod` 4
      padCount = if remainder == 0 then 0 else 4 - remainder

  if padCount > 0 && BS.last serializedOps == 0xff
    then do
      putByteString (BS.init serializedOps)
      replicateM_ padCount (putWord8 0)
      putWord8 0xff
    else do
      putByteString serializedOps
