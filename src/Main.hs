{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import ADHD.Config
import ADHD.DHCP
import ADHD.DHCP.Generator
import ADHD.DHCP.Types
import ADHD.Logging
import Control.Monad
import Control.Monad.RWS.CPS
import Net.IPv4 qualified as IP
import Options.Applicative
import System.Exit
import System.Posix.User
import Prelude hiding (log)

-- | Record for parsing CLI options
data CLI = CLI
  { configPath :: FilePath,
    bindPlease :: Bool,
    dryRun :: Bool
  }

-- | CLI options parser
cli :: Parser CLI
cli =
  CLI
    <$> strOption
      ( long "config"
          <> short 'c'
          <> metavar "FILE"
          <> help "Path to config file"
          <> showDefault
          <> value "config.dhall"
      )
    <*> switch
      ( long "user"
          <> short 'u'
          <> help "Try to launch instead of paniching when ran from non-root"
      )
    <*> switch
      ( long "dry-run"
          <> short 'g'
          <> help "Generate an IP address and quit"
      )

main :: IO ()
main = do
  app <-
    execParser
      . info (helper <*> cli)
      $ fullDesc <> header "ADHD - the best DHCP server ever made"

  when (not (app.bindPlease || app.dryRun)) do
    uid <- getEffectiveUserID
    when (uid /= 0) $ do
      log Error "Please, run the program from root (or use --user flag if you are brave enough)"
      exitFailure

  st <- initialize $ not app.dryRun
  cfg <- readConfig app.configPath

  void $
    runRWST
      (sanityCheck >> if app.dryRun then dryRun else forever loop)
      cfg
      st

-- | Procedure for dry running
dryRun :: DHCPM ()
dryRun = do
  generateIP >>= \case
    Nothing -> do
      liftIO do
        log Error "Could not generate IP: Generator returned Nothing"
        exitFailure
    Just ip -> liftIO $ IP.print ip
