{-# LANGUAGE OverloadedRecordDot #-}

module ADHD.DHCP.Generator where

import ADHD.Config
import ADHD.DHCP.Types
import Control.Monad.RWS.CPS
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word
import Net.IPv4
import System.Random.Shuffle
import Text.Read (readMaybe)

generateIp :: DHCPM (Maybe IPv4)
generateIp = do
  ServerState {ipMap, pendingMap} <- get
  cfg <- ask
  let usedIps =
        Set.fromList $
          cfg.gateway
            : cfg.serverIp
            : concatMap M.elems [getIPMap ipMap, pendingMap]
              <> cfg.occupiedIps
      candidates = beautifulIps cfg.network cfg.beautifulStrings

  find (`Set.notMember` usedIps) <$> lift (shuffleM candidates)

beautifulIps :: IPv4Range -> [Text] -> [IPv4]
beautifulIps network parts =
  nub $
    [ ip
    | ss <- strings octets parts,
      spl <- split octets ss,
      host <- maybeToList $ traverse parseOctet spl,
      ip <- maybeToList $ hostToIp network host
    ]
  where
    octets = hostOctets network

strings :: Int -> [Text] -> [Text]
strings octets =
  filter goodLength . nub . fmap T.concat . glue
  where
    goodLength s =
      let len = T.length s
       in len >= octets && len <= octets * 3

glue :: [a] -> [[a]]
glue xs =
  [ take n rest
  | rest <- tails xs,
    n <- [1 .. length rest]
  ]

split :: Int -> Text -> [[Text]]
split 0 s
  | T.null s = [[]]
  | otherwise = []
split count s =
  [ part : restParts
  | len <- [1 .. 3],
    let (part, rest) = T.splitAt len s,
    T.length part == len,
    restParts <- split (count - 1) rest
  ]

parseOctet :: Text -> Maybe Word8
parseOctet s = do
  n <- readMaybe @Int (T.unpack s)
  if n <= 255 then Just $ fromIntegral n else Nothing

hostOctets :: IPv4Range -> Int
hostOctets = (`div` 8) . (32 -) . fromIntegral . ipv4RangeLength

hostToIp :: IPv4Range -> [Word8] -> Maybe IPv4
hostToIp network host =
  case (hostOctets network, toOctets $ ipv4RangeBase network, host) of
    (4, (_, _, _, _), [a, b, c, d]) ->
      valid $ ipv4 a b c d
    (3, (a, _, _, _), [b, c, d]) ->
      valid $ ipv4 a b c d
    (2, (a, b, _, _), [c, d]) ->
      valid $ ipv4 a b c d
    (1, (a, b, c, _), [d]) ->
      valid $ ipv4 a b c d
    _ ->
      Nothing
  where
    valid ip
      | network `contains` ip = Just ip
      | otherwise = Nothing
