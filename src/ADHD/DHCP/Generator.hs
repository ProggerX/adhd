module ADHD.DHCP.Generator where

import ADHD.Config
import ADHD.DHCP.Types
import Control.Monad.RWS.CPS
import Data.Map qualified as M
import Data.Set qualified as S
import Net.IPv4

generateIp :: DHCPM (Maybe IPv4)
generateIp = do
  ServerState {ipMap, pendingMap} <- get
  Configuration {network, gateway, occupiedIps} <- ask
  let usedIps =
        S.fromList $
          gateway
            : concatMap M.elems [ipMap, pendingMap]
              <> occupiedIps

  pure $ case filter (not . (`S.member` usedIps))
    . drop 1
    . init
    $ toList network of
    [] -> Nothing
    (x : _) -> Just x
