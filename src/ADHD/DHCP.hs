{-# LANGUAGE BlockArguments #-}
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
import Data.Binary qualified as Binary
import Data.Binary.Put
import Data.Bits
import Data.ByteString (ByteString, toStrict)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
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

-- | Supported requests
data Request
  = Discover
  | Request IPv4

-- | Supported responses
data Response
  = Offer IPv4
  | Nak
  | Ack IPv4
  | None

-- | Parse request from raw message
parseRequest :: RawMessage -> Maybe Request
parseRequest msg =
  getMessageType msg >>= \case
    1 -> Just Discover
    3 -> Request <$> getRequestedIP msg
    _ -> Nothing

-- | Main loop
loop :: DHCPM ()
loop =
  receive >>= \case
    Nothing -> pure ()
    Just (raw@RawMessage {chaddr}, addr) ->
      case parseRequest raw of
        Just msg -> process chaddr msg >>= respond addr raw
        Nothing -> pure ()

-- | Process Request and give a Response
process ::
  -- | Client hardware address got from raw message
  ByteString ->
  Request ->
  DHCPM Response
process chaddr = \case
  Discover -> do
    liftIO $ log Info "Got discover..."
    ServerState {ipMap} <- get
    gip <- generateIP
    pure $ maybe None Offer $ getIPMap ipMap M.!? chaddr <|> gip
  Request ip -> do
    liftIO $ log Info "Got request..."
    ServerState {ipMap, pendingMap} <- get
    pure $ case getIPMap ipMap M.!? chaddr <|> pendingMap M.!? chaddr of
      Just ip' | ip' == ip -> Ack ip
      _ -> Nak

-- | Send Response to socket
respond ::
  -- | Address needed to work under relay
  S.SockAddr ->
  -- | Raw message got from the client is updated to the responding message
  RawMessage ->
  Response ->
  DHCPM ()
respond _ _ None = pure ()
respond addr rawMsg resp = do
  cfg <- ask
  st <- get
  let msg =
        rawMsg
          { ciaddr = ipv4 0 0 0 0,
            yiaddr = ipv4 0 0 0 0,
            siaddr = cfg.serverIP
          }
      offerMsg ip = msg {yiaddr = ip}
      bareOptions t =
        [ MessageType t,
          ServerIdentity $ ipToBs cfg.serverIP
        ]
      offerOptions t =
        bareOptions t
          <> [ Gateway cfg.gateway,
               NetworkMask $ ipv4RangeLength cfg.network,
               DNS cfg.dns,
               LeaseDuration 0xffffffff,
               BroadcastAddress $
                 ipv4RangeBase cfg.network
                   .|. ( complement
                           . maskToIp
                           . fromIntegral
                           $ ipv4RangeLength cfg.network
                       )
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
          { ipMap = IPMap . M.insert rawMsg.chaddr ip $ getIPMap st.ipMap,
            pendingMap = M.delete rawMsg.chaddr st.pendingMap
          }
      syncState
      info ["Acknowledged that ", show ip, " belongs to ", showMac rawMsg.chaddr]
    Nak -> info ["Sent NAK to ", showMac rawMsg.chaddr]
  where
    info = liftIO . log Info . concat

-- | Initialize ServerState
initialize ::
  -- | Whether to bind socket. Needed for dry-running
  Bool ->
  IO ServerState
initialize withSocket = do
  s <- S.socket AF_INET Datagram defaultProtocol

  when withSocket do
    setSocketOption s ReuseAddr 1
    setSocketOption s Broadcast 1
    bind s $ SockAddrInet 67 0

  let path = ".adhdMap.bin"
  mapExists <- doesFileExist path
  ipMap <-
    if mapExists
      then Binary.decode . LBS.fromStrict <$> BS.readFile path
      else pure mempty

  pure ServerState {socket = s, ipMap, pendingMap = mempty}

-- | Write current state to the disk
syncState :: DHCPM ()
syncState = do
  ServerState {ipMap} <- get
  liftIO . BS.writeFile ".adhdMap.bin" . toStrict $ Binary.encode ipMap

-- | Normalize server state on launch
sanityCheck :: DHCPM ()
sanityCheck = do
  Configuration {occupiedIPs} <- ask
  st@ServerState {ipMap} <- get
  put
    st
      { ipMap =
          IPMap
            . M.filter
              ( not
                  . (`Set.member` Set.fromList occupiedIPs)
              )
            $ getIPMap ipMap
      }
  syncState
