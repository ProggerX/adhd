{-# OPTIONS -Wno-orphans #-}

module ADHD.DHCP.Types where

import ADHD.Config
import Control.Monad.RWS.CPS
import Data.Binary
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.List
import Data.Map.Strict (Map)
import GHC.Generics
import Net.IPv4
import Network.Socket
import Text.Printf

-- | Main server monad
type DHCPM = RWST Configuration () ServerState IO

data ServerState = ServerState
  { ipMap :: IPMap,
    pendingMap :: Map ByteString IPv4,
    socket :: Socket
  }

-- | RFC-accurate raw message type
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

-- | Helper type for parsing options
data RawOption
  = Pad
  | End
  | Option Word8 ByteString
  deriving (Show)

-- | Abstracted option type
data Option
  = MessageType Word8
  | ServerIdentity ByteString
  | Gateway IPv4
  | NetworkMask Word8
  | LeaseDuration Word32
  | DNS [IPv4]
  | BroadcastAddress IPv4

-- | IP map serialized and written to disk
newtype IPMap = IPMap {getIPMap :: Map ByteString IPv4}
  deriving (Semigroup, Monoid, Generic, Binary)

-- | Orphan instance to serialize IPs
instance Binary IPv4

-- | Convert "client hardware address" to pretty MAC
showMac :: ByteString -> String
showMac =
  intercalate ":"
    . fmap (printf "%02x")
    . dropWhileEnd (== 0)
    . BS.unpack

ipToBs :: IPv4 -> ByteString
ipToBs =
  BS.toStrict
    . toLazyByteString
    . word32BE
    . getIPv4
