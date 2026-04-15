module ADHD.DHCP.Types where

import ADHD.Config
import Control.Monad.RWS.CPS
import Data.Binary
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.List
import Data.Map.Strict (Map)
import Net.IPv4
import Network.Socket
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
