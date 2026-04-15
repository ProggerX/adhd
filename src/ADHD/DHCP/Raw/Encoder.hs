{-# LANGUAGE RecordWildCards #-}

module ADHD.DHCP.Raw.Encoder where

import ADHD.DHCP.Types
import Data.Binary
import Data.Binary.Put
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Net.IPv4

putCookie :: Put
putCookie = putWord32be 0x63825363

putOption' :: RawOption -> Put
putOption' (Option t v) = do
  putWord8 t
  putWord8 $ fromIntegral $ BS.length v
  putByteString v
putOption' End = putWord8 255
putOption' Pad = putWord8 0

putOption :: Word8 -> ByteString -> Put
putOption = (putOption' .) . Option

maskToIp :: Int -> IPv4
maskToIp n = IPv4 $ 0xffffffff `shiftL` (32 - n)

putIP :: IPv4 -> Put
putIP = putWord32be . getIPv4

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

putOptions :: [RawOption] -> Put
putOptions = mapM_ putOption'
