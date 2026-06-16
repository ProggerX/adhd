{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ADHD.DHCP.Generator where

import ADHD.Config
import ADHD.DHCP.Types
import Control.Monad.RWS.CPS
import Data.Containers.ListUtils
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
import Prelude hiding (log)

-- | Generate IP. Returns Nothing when all beautiful IPs are occupied
generateIP :: DHCPM (Maybe IPv4)
generateIP = do
  ServerState {ipMap, pendingMap} <- get
  cfg <- ask
  let usedIPs =
        Set.fromList $
          cfg.gateway
            : cfg.serverIP
            : concatMap M.elems [getIPMap ipMap, pendingMap]
              <> cfg.occupiedIPs
      candidates = beautifulIPs cfg.network cfg.beautifulStrings

  find (`Set.notMember` usedIPs) <$> lift (shuffleM candidates)

-- | Generate list of all beautiful IPs
beautifulIPs :: IPv4Range -> [Text] -> [IPv4]
beautifulIPs network parts =
  nubOrd $
    do
      ss <- strings octets parts
      spl <- split octets ss
      os <- maybeToList $ traverse parseOctet spl
      maybeToList $ hostToIP network os
  where
    octets = hostOctets network

-- | Generate list of beautiful strings to split into IPs
strings :: Int -> [Text] -> [Text]
strings octets =
  filter goodLength . nubOrd . fmap T.concat . subsPerms
  where
    goodLength s =
      let len = T.length s
       in len >= octets && len <= octets * 3

-- | Get permutations of subsets of a list
subsPerms :: [a] -> [[a]]
subsPerms xs =
  concatMap
    permutations
    [ take n rest
    | rest <- tails xs,
      n <- [1 .. length rest]
    ]

-- | Split string into list of parts of 1..3 lengths
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

-- | Parse an octet. Returns Nothing if it is not numeric or bigger than 255
parseOctet :: Text -> Maybe Word8
parseOctet s = do
  n <- readMaybe @Int (T.unpack s)
  if n <= 255 then Just $ fromIntegral n else Nothing

-- | Get a host octets count from IPv4Range
hostOctets :: IPv4Range -> Int
hostOctets = (`div` 8) . (32 -) . fromIntegral . ipv4RangeLength

-- | Generate final address from host octets
hostToIP :: IPv4Range -> [Word8] -> Maybe IPv4
hostToIP network host =
  let (a, b, c, d) = toOctets (ipv4RangeBase network)
      full = take (4 - hostOctets network) [a, b, c, d] <> host
   in case full of
        [a', b', c', d']
          | network `contains` ipv4 a' b' c' d' -> Just $ ipv4 a' b' c' d'
        _ -> Nothing
