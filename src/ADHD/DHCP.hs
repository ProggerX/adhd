{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module ADHD.DHCP where

import ADHD.Config
import ADHD.DHCP.Generator
import ADHD.DHCP.Raw
import ADHD.Logging
import Control.Applicative
import Control.Monad
import Control.Monad.RWS.CPS
import Data.Binary hiding (get, put)
import Data.Binary.Put
import Data.ByteString (ByteString, pack, toStrict)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Maybe
import Net.IPv4 as IP hiding (encode)
import Network.Socket hiding (socket)
import Network.Socket qualified as S
import Network.Socket.ByteString
import System.Directory
import Prelude hiding (log)

data Request
  = Discover ByteString
  | Request ByteString IPv4

data Response
  = Offer ByteString IPv4
  | Nak
  | Ack ByteString IPv4

parseRequest :: RawMessage -> Maybe Request
parseRequest msg@RawMessage {..} =
  getMessageType msg >>= \case
    1 -> Just $ Discover chaddr
    3 -> Request chaddr <$> getRequestedIp msg
    _ -> Nothing

loop :: DHCPM ()
loop =
  recieve >>= \case
    Nothing -> pure ()
    Just (raw, addr) ->
      case parseRequest raw of
        Just msg -> respond addr raw msg
        Nothing -> pure ()

respond :: S.SockAddr -> RawMessage -> Request -> DHCPM ()
respond addr raw = \case
  Discover chaddr -> do
    liftIO $ log Info "Got discover..."
    ServerState {..} <- get
    case ipMap M.!? chaddr of
      Just ip -> offer addr raw ip
      Nothing ->
        generateIp >>= \case
          Nothing -> pure ()
          Just ip -> offer addr raw ip
  Request chaddr ip -> do
    liftIO $ log Info "Got request..."
    ServerState {..} <- get
    case ipMap M.!? chaddr <|> pendingMap M.!? chaddr of
      Just ip' | ip' == ip -> ack addr raw ip
      _ -> nak addr raw

-- TODO: cleanup
offer :: S.SockAddr -> RawMessage -> IPv4 -> DHCPM ()
offer addr RawMessage {..} ip = do
  liftIO $ log Info $ "Offering " <> show ip <> " to " <> showMac chaddr
  Configuration {..} <- ask
  st@ServerState {..} <- get
  put st {pendingMap = M.insert chaddr ip pendingMap}
  syncState

  let bytes = toStrict . runPut $ do
        putWord8 2
        putWord8 htype
        putWord8 hlen
        putWord8 0
        putByteString xid
        putByteString secs
        putByteString flags

        putWord32be 0
        putWord32be $ getIPv4 ip
        putWord32be 0
        putWord32be $ getIPv4 giaddr

        putByteString chaddr
        putByteString $ BS.replicate 64 0
        putByteString $ BS.replicate 128 0

        putCookie
        putOption 53 $ pack [2]
        putOption 54 $ ipToBs serverIp
        putOption 1 $ ipToBs netMask
        putOption 3 $ ipToBs gateway
        putOption 6 $ BS.concat $ ipToBs <$> dns
        putOption 51 $ pack [0xff, 0xff, 0xff, 0xff]
        putOption' End

  void $ liftIO $ sendTo socket bytes addr

ack :: S.SockAddr -> RawMessage -> IPv4 -> DHCPM ()
ack addr RawMessage {..} ip = do
  liftIO $ log Info $ "Acknowledging, " <> show ip <> " belongs to " <> showMac chaddr
  Configuration {..} <- ask

  st@ServerState {..} <- get
  put
    st
      { ipMap = M.insert chaddr ip ipMap,
        pendingMap = M.delete chaddr pendingMap
      }
  syncState

  let bytes = toStrict . runPut $ do
        putWord8 2
        putWord8 htype
        putWord8 hlen
        putWord8 0
        putByteString xid
        putByteString secs
        putByteString flags

        putWord32be 0
        putWord32be $ getIPv4 ip
        putWord32be 0
        putWord32be $ getIPv4 giaddr

        putByteString chaddr
        putByteString $ BS.replicate 64 0
        putByteString $ BS.replicate 128 0

        putCookie
        putOption 53 $ pack [5]
        putOption 54 $ ipToBs serverIp
        putOption 1 $ ipToBs netMask
        putOption 3 $ ipToBs gateway
        putOption 6 $ BS.concat $ ipToBs <$> dns
        putOption 51 $ pack [0xff, 0xff, 0xff, 0xff]
        putOption' End
  void $ liftIO $ sendTo socket bytes addr

nak :: S.SockAddr -> RawMessage -> DHCPM ()
nak addr RawMessage {..} = do
  liftIO $ log Info $ "Sending NAK to " <> showMac chaddr
  Configuration {..} <- ask
  ServerState {socket} <- get
  let bytes = toStrict . runPut $ do
        putWord8 2
        putWord8 htype
        putWord8 hlen
        putWord8 0
        putByteString xid
        putByteString secs
        putByteString flags

        putWord32be 0
        putWord32be 0
        putWord32be 0
        putWord32be $ getIPv4 giaddr

        putByteString chaddr
        putByteString $ BS.replicate 64 0
        putByteString $ BS.replicate 128 0

        putCookie
        putOption 53 $ pack [6]
        putOption 54 $ ipToBs serverIp
        putOption' End
  void $ liftIO $ sendTo socket bytes addr

initialize :: IO ServerState
initialize = do
  s <- S.socket AF_INET Datagram defaultProtocol
  setSocketOption s ReuseAddr 1
  setSocketOption s Broadcast 1
  bind s $ SockAddrInet 67 0

  mapExists <- doesFileExist "ipMap.bin"
  ipMap <-
    if mapExists
      then read <$> readFile "ipMap.bin"
      else pure mempty

  pure ServerState {socket = s, ipMap, pendingMap = mempty}

syncState :: DHCPM ()
syncState = do
  ServerState {ipMap} <- get
  liftIO $ writeFile "ipMap.bin" $ show ipMap
