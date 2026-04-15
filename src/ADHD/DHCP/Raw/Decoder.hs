module ADHD.DHCP.Raw.Decoder where

import ADHD.DHCP.Types
import Data.Binary
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.Maybe
import Net.IPv4

getMessage :: Get RawMessage
getMessage =
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
