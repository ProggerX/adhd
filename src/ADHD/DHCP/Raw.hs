{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module ADHD.DHCP.Raw where

import ADHD.DHCP.Raw.Decoder
import ADHD.DHCP.Raw.Encoder
import ADHD.DHCP.Types
import Control.Monad.RWS.CPS
import Data.Binary.Get
import Data.ByteString (fromStrict, pack, toStrict)
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Net.IPv4
import Network.Socket as S
import Network.Socket.ByteString

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

  pure $ case runGetOrFail getMessage $ fromStrict bs of
    Left _ -> Nothing
    Right (_, _, msg) -> Just (msg, replyDestAddr msg)

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
