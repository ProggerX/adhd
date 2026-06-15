{-# LANGUAGE OverloadedStrings #-}

module Main where

import ADHD.Config
import ADHD.DHCP
import ADHD.DHCP.Generator
import ADHD.Logging
import Control.Monad
import Control.Monad.RWS.CPS
import System.Exit
import System.Posix.User
import Prelude hiding (log)

main :: IO ()
main = do
  uid <- getEffectiveUserID
  when (uid /= 0) $ do
    log Error "Please, run the program from root"
    exitFailure
  st <- initialize
  cfg <- readConfig
  void $
    runRWST
      (sanityCheck >> forever loop)
      cfg
      st

-- NOTE: for debuging
genIP :: IO ()
genIP = do
  st <- initialize
  cfg <- readConfig
  (a, _, _) <-
    runRWST
      (sanityCheck >> generateIp)
      cfg
      st
  print a
