{-# LANGUAGE OverloadedRecordDot #-}

module ADHD.DHCP.Generator where

import ADHD.Config
import ADHD.DHCP.Types
import Control.Monad
import Control.Monad.RWS.CPS
import Data.Map qualified as M
import Data.Set qualified as S
import Net.IPv4
import System.Random.Shuffle

generateIp :: DHCPM (Maybe IPv4)
generateIp = do
  ServerState {ipMap, pendingMap} <- get
  cfg <- ask
  [bytes1, bytes2, bytes3, bytes4] <-
    liftIO
      . replicateM 4
      $ shuffleM cfg.beautifulBytes
  let usedIps =
        S.fromList $
          cfg.gateway
            : concatMap M.elems [ipMap, pendingMap]
              <> cfg.occupiedIps
      (pr1, pr2, pr3, pr4) = toOctets $ ipv4RangeBase cfg.network

      pr `but` ps = if pr == 0 then ps else [pr]

      allowedIps =
        filter
          (cfg.network `contains`)
          [ ipv4 a b c d
          | a <- pr1 `but` bytes1,
            b <- pr2 `but` bytes2,
            c <- pr3 `but` bytes3,
            d <- pr4 `but` bytes4
          ]

  pure $ case filter (not . (`S.member` usedIps)) $
    allowedIps of
    [] -> Nothing
    (x : _) -> Just x
