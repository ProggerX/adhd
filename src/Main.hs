{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ADHD.Config
import ADHD.DHCP
import ADHD.Logging
import Control.Monad
import Control.Monad.RWS.CPS
import Prelude hiding (log)

main :: IO ()
main = do
  st <- initialize
  ecfg <- readConfig
  case ecfg of
    Left e ->
      log Error $ "Error while reading config.dhall: " <> e
    Right cfg ->
      void $
        runRWST
          (sanityCheck >> forever loop)
          cfg
          st
