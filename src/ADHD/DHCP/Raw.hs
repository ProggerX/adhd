{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module ADHD.DHCP.Raw where

-- TODO: Split this module into 99999 shards

import ADHD.Config
import Control.Monad.RWS.CPS
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString (ByteString, fromStrict, pack, toStrict)
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import Net.IPv4
import Network.Socket as S
import Network.Socket.ByteString
import Text.Printf

type DHCPM = RWST Configuration () ServerState IO

data ServerState = ServerState
  { ipMap :: Map ByteString IPv4,
    pendingMap :: Map ByteString IPv4,
    socket :: Socket
  }

data RawMessage = RawMessage
  { op :: Word8,
    htype :: Word8,
    hlen :: Word8,
    hops :: Word8,
    xid :: ByteString,
    secs :: ByteString,
    flags :: ByteString,
    ciaddr :: IPv4,
    yiaddr :: IPv4,
    siaddr :: IPv4,
    giaddr :: IPv4,
    chaddr :: ByteString,
    sname :: ByteString,
    file :: ByteString,
    options :: [RawOption]
  }
  deriving (Show)

data RawOption
  = Pad
  | End
  | Option Word8 ByteString
  deriving (Show)

data Option
  = MessageType Word8
  | ServerIdentity ByteString
  | Gateway IPv4
  | NetworkMask Word8
  | LeaseDuration Word32
  | DNS [IPv4]

showMac :: ByteString -> String
showMac =
  intercalate ":"
    . fmap (printf "%02x")
    . reverse
    . dropWhile (== 0)
    . reverse
    . BS.unpack

ipToBs :: IPv4 -> ByteString
ipToBs =
  BS.toStrict
    . toLazyByteString
    . word32BE
    . getIPv4

replyDestAddr :: RawMessage -> SockAddr
replyDestAddr msg
  | getIPv4 msg.giaddr /= 0 =
      SockAddrInet 67 $ ipToHostAddr msg.giaddr
  | getIPv4 msg.ciaddr /= 0 =
      SockAddrInet 68 $ ipToHostAddr msg.ciaddr
  | otherwise =
      SockAddrInet 68 0xffffffff
  where
    ipToHostAddr = tupleToHostAddress . toOctets

recieve :: DHCPM (Maybe (RawMessage, SockAddr))
recieve = do
  s <- gets (.socket)
  (bs, _addr) <- liftIO $ recvFrom s 1500

  pure $ case runGetOrFail rawMessage $ fromStrict bs of
    Left _ -> Nothing
    Right (_, _, msg) -> Just (msg, replyDestAddr msg)

rawMessage :: Get RawMessage
rawMessage =
  RawMessage
    <$> getWord8
    <*> getWord8
    <*> getWord8
    <*> getWord8
    <*> getByteString 4
    <*> getByteString 2
    <*> getByteString 2
    <*> getIP
    <*> getIP
    <*> getIP
    <*> getIP
    <*> getByteString 16
    <*> getByteString 64
    <*> getByteString 128
    <*> (getCookie >> getOptions)

getIP :: Get IPv4
getIP = IPv4 <$> getWord32be

getCookie :: Get Word32
getCookie = do
  ck <- getWord32be
  if ck == 0x63825363
    then
      pure ck
    else fail "Wrong cookie"

getOption :: Get RawOption
getOption = do
  tag <- getWord8
  case tag of
    0 -> pure Pad
    255 -> pure End
    _ -> do
      len <- getWord8
      val <- getByteString $ fromIntegral len
      pure $ Option tag val

getOptions :: Get [RawOption]
getOptions = go
  where
    go = do
      opt <- getOption
      case opt of
        End -> pure [End]
        _ -> (opt :) <$> go

getMessageType :: RawMessage -> Maybe Word8
getMessageType RawMessage {options} =
  listToMaybe $
    mapMaybe
      ( \case
          Option 53 bs | BS.length bs == 1 -> Just $ BS.head bs
          _ -> Nothing
      )
      options

getRequestedIp :: RawMessage -> Maybe IPv4
getRequestedIp RawMessage {options} =
  listToMaybe $
    mapMaybe
      ( \case
          Option 50 bs | BS.length bs == 4 ->
            case BS.unpack bs of
              [a, b, c, d] ->
                Just $ fromOctets a b c d
              _ -> Nothing
          _ -> Nothing
      )
      options

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

withOptions :: RawMessage -> [Option] -> RawMessage
withOptions msg ops = msg {options = map packOption ops <> [End]}
  where
    packOption = \case
      MessageType n -> Option 53 $ pack [n]
      ServerIdentity bs -> Option 54 bs
      NetworkMask w ->
        Option 1
          . ipToBs
          . maskToIp
          $ fromIntegral
            w
      Gateway ip -> Option 3 $ ipToBs ip
      LeaseDuration w ->
        Option 51
          . toStrict
          . toLazyByteString
          $ word32BE w
      DNS ips -> Option 6 $ BS.concat $ ipToBs <$> ips
