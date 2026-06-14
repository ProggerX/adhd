{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS -Wno-orphans #-}

module ADHD.Config where

import Data.Text
import Data.Text.IO qualified as TIO
import Data.Word
import Dhall
import Dhall.Core
import Net.IPv4
import System.Directory

data Configuration = Configuration
  { serverIp :: IPv4,
    gateway :: IPv4,
    network :: IPv4Range,
    occupiedIps :: [IPv4],
    dns :: [IPv4],
    beautifulBytes :: [Word8]
  }
  deriving (FromDhall, Generic, Show)

instance FromDhall IPv4 where
  autoWith _ = textWith decode

instance FromDhall IPv4Range where
  autoWith _ = textWith decodeRange

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

readConfig :: IO (Either String Configuration)
readConfig = do
  exists <- doesFileExist "config.dhall"
  if exists
    then do
      txt <- TIO.readFile "config.dhall"
      Right <$> input (auto @Configuration) txt
    else
      pure $ Left "config file does not exist"
