{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module ADHD.DHCP where

import ADHD.Config
import ADHD.DHCP.Generator
import ADHD.DHCP.Raw
import ADHD.DHCP.Raw.Decoder
import ADHD.DHCP.Raw.Encoder
import ADHD.DHCP.Types
import ADHD.Logging
import Control.Applicative
import Control.Monad
import Control.Monad.RWS.CPS
import Data.Binary.Put
import Data.ByteString (ByteString, toStrict)
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
  = Discover
  | Request IPv4

data Response
  = Offer IPv4
  | Nak
  | Ack IPv4
  | None

parseRequest :: RawMessage -> Maybe Request
parseRequest msg =
  getMessageType msg >>= \case
    1 -> Just $ Discover
    3 -> Request <$> getRequestedIp msg
    _ -> Nothing

loop :: DHCPM ()
loop =
  recieve >>= \case
    Nothing -> pure ()
    Just (raw@RawMessage {chaddr}, addr) ->
      case parseRequest raw of
        Just msg -> process chaddr msg >>= respond addr raw
        Nothing -> pure ()

process :: ByteString -> Request -> DHCPM Response
process chaddr = \case
  Discover -> do
    liftIO $ log Info "Got discover..."
    ServerState {ipMap} <- get
    gip <- generateIp
    pure $ case ipMap M.!? chaddr <|> gip of
      Just ip -> Offer ip
      Nothing -> None
  Request ip -> do
    liftIO $ log Info "Got request..."
    ServerState {ipMap, pendingMap} <- get
    pure $ case ipMap M.!? chaddr <|> pendingMap M.!? chaddr of
      Just ip' | ip' == ip -> Ack ip
      _ -> Nak

respond :: S.SockAddr -> RawMessage -> Response -> DHCPM ()
respond _ _ None = pure ()
respond addr rawMsg resp = do
  cfg <- ask
  st <- get
  let msg =
        rawMsg
          { ciaddr = ipv4 0 0 0 0,
            yiaddr = ipv4 0 0 0 0,
            siaddr = ipv4 0 0 0 0
          }
      offerMsg ip = msg {yiaddr = ip}
      bareOptions t =
        [ MessageType t,
          ServerIdentity $ ipToBs cfg.serverIp
        ]
      offerOptions t =
        bareOptions t
          <> [ Gateway cfg.gateway,
               NetworkMask $ ipv4RangeLength cfg.network,
               DNS cfg.dns,
               LeaseDuration 0xffffff
             ]
  void
    . liftIO
    . (flip $ sendTo st.socket) addr
    . toStrict
    . runPut
    . putMessage
    $ case resp of
      Nak -> msg `withOptions` bareOptions 6
      Offer ip -> offerMsg ip `withOptions` offerOptions 2
      Ack ip -> offerMsg ip `withOptions` offerOptions 5

  case resp of
    Offer ip -> do
      put st {pendingMap = M.insert rawMsg.chaddr ip st.pendingMap}
      syncState
      info ["Offered ", show ip, " to ", showMac rawMsg.chaddr]
    Ack ip -> do
      put
        st
          { ipMap = M.insert rawMsg.chaddr ip st.ipMap,
            pendingMap = M.delete rawMsg.chaddr st.pendingMap
          }
      syncState
      info ["Acknowledged that ", show ip, " belongs to ", showMac rawMsg.chaddr]
    Nak -> info ["Sent NAK to ", showMac rawMsg.chaddr]
  where
    info = liftIO . log Info . concat

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
