{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS -Wno-orphans #-}

module ADHD.Config where

import Data.Text
import Data.Text.IO qualified as TIO
import Dhall
import Dhall.Core
import Net.IPv4
import System.Directory

-- | ADHD configuration.
data Configuration = Configuration
  { -- | serverIP is needed by the protocol for ServerIdentity option (adhd still listens on ANY interface)
    serverIP :: IPv4,
    gateway :: IPv4,
    network :: IPv4Range,
    -- | occupiedIPs + gateway + serverIP are excluded from the set for generation
    occupiedIPs :: [IPv4],
    dns :: [IPv4],
    -- | beautifulStrings are string patterns used for generation. ONLY them are used for generation.
    beautifulStrings :: [Text]
  }
  deriving (FromDhall, Generic, Show)

instance FromDhall IPv4 where
  autoWith _ = textWith decode

instance FromDhall IPv4Range where
  autoWith _ = textWith decodeRange

-- | Helper to decode IPs from dhall text
textWith :: (Text -> Maybe a) -> Decoder a
textWith parse =
  Decoder
    { extract = \case
        TextLit (Chunks [] t) ->
          Prelude.maybe (extractError $ "Cannot parse: " <> t) pure $ parse t
        expr -> typeError expected' expr,
      expected = expected'
    }
  where
    expected' = expected strictText

-- | Procedure for reading configuration
readConfig ::
  -- | Where to get configuration
  FilePath ->
  IO Configuration
readConfig path = do
  exists <- doesFileExist path
  if exists
    then TIO.readFile path >>= input auto
    else fail $ "config file " <> path <> " does not exist"
