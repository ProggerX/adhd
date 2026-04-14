{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ADHD.Config
import ADHD.DHCP
import Control.Monad
import Control.Monad.RWS.CPS
import Data.Maybe
import Data.Text
import Net.IPv4 hiding (print)

ip :: Text -> IPv4
ip = fromJust . decode

main :: IO ()
main = do
  st <- initialize
  void $
    runRWST
      (forever loop)
      ( Configuration
          { gateway = ip "13.37.0.1",
            serverIp = ip "13.37.22.8",
            netMask = ip "255.255.0.0",
            dns = [ip "13.37.67.67"]
          }
      )
      st
