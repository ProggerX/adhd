module ADHD.DHCP.Generator where

import ADHD.DHCP.Raw
import Control.Monad.RWS.CPS
import Data.Map qualified as M
import Net.IPv4

-- HACK: hardcoded generation
generateIp :: DHCPM (Maybe IPv4)
generateIp = do
  ServerState {ipMap, pendingMap} <- get
  let usedIps = concatMap M.elems [ipMap, pendingMap]
  pure $ case filter (not . (`elem` usedIps)) $
    fromOctets 13 37 <$> [52, 69] <*> [52, 69] of
    [] -> Nothing
    (x : _) -> Just x
