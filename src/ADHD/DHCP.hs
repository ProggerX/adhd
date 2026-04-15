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
import Data.Set qualified as Set
import Net.IPv4 as IP hiding (encode)
import Network.Socket hiding (socket)
import Network.Socket qualified as S
import Network.Socket.ByteString
import System.Directory
import System.IO
import Prelude hiding (log)

data Request
  = Discover ByteString
  | Request ByteString IPv4

data Response
  = Offer IPv4
  | Nak
  | Ack IPv4
  | None

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
        Just msg -> process msg >>= respond addr raw
        Nothing -> pure ()

process :: Request -> DHCPM Response
process = \case
  Discover chaddr -> do
    liftIO $ log Info "Got discover..."
    ServerState {..} <- get
    gip <- generateIp
    pure $ case ipMap M.!? chaddr <|> gip of
      Just ip -> Offer ip
      Nothing -> None
  Request chaddr ip -> do
    liftIO $ log Info "Got request..."
    ServerState {..} <- get
    pure $ case ipMap M.!? chaddr <|> pendingMap M.!? chaddr of
      Just ip' | ip' == ip -> Ack ip
      _ -> Nak

-- FIXME: move serialization to another module and add interface for it
respond :: S.SockAddr -> RawMessage -> Response -> DHCPM ()
respond _ _ None = pure ()
respond addr RawMessage {..} resp = do
  Configuration {..} <- ask
  st@ServerState {..} <- get
  void
    . liftIO
    . (flip $ sendTo socket) addr
    . toStrict
    . runPut
    $ case resp of
      Nak -> do
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
      Offer ip -> do
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
        putOption 1
          . ipToBs
          . maskToIp
          . fromIntegral
          $ ipv4RangeLength network
        putOption 3 $ ipToBs gateway
        putOption 6 $ BS.concat $ ipToBs <$> dns
        putOption 51 $ pack [0xff, 0xff, 0xff, 0xff]
        putOption' End
      Ack ip -> do
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
        putOption 1
          . ipToBs
          . maskToIp
          . fromIntegral
          $ ipv4RangeLength network
        putOption 3 $ ipToBs gateway
        putOption 6 $ BS.concat $ ipToBs <$> dns
        putOption 51 $ pack [0xff, 0xff, 0xff, 0xff]
        putOption' End

  case resp of
    Offer ip -> do
      put st {pendingMap = M.insert chaddr ip pendingMap}
      syncState
    Ack ip -> do
      put
        st
          { ipMap = M.insert chaddr ip ipMap,
            pendingMap = M.delete chaddr pendingMap
          }
      syncState
    _ -> pure ()

initialize :: IO ServerState
initialize = do
  s <- S.socket AF_INET Datagram defaultProtocol
  setSocketOption s ReuseAddr 1
  setSocketOption s Broadcast 1
  bind s $ SockAddrInet 67 0

  mapExists <- doesFileExist "ipMap.bin"
  ipMap <-
    if mapExists
      then read <$> readFile' "ipMap.bin"
      else pure mempty

  pure ServerState {socket = s, ipMap, pendingMap = mempty}

syncState :: DHCPM ()
syncState = do
  ServerState {ipMap} <- get
  liftIO $ writeFile "ipMap.bin" $ show ipMap

sanityCheck :: DHCPM ()
sanityCheck = do
  Configuration {occupiedIps} <- ask
  st@ServerState {ipMap} <- get
  put
    st
      { ipMap =
          M.filter
            ( not
                . (`Set.member` Set.fromList occupiedIps)
            )
            ipMap
      }
  syncState
