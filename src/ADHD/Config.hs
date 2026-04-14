{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module ADHD.Config where

import Data.Text
import Data.Text.IO qualified as TIO
import Dhall
import Net.IPv4
import System.Directory

data Configuration = Configuration
  { serverIp :: IPv4,
    gateway :: IPv4,
    network :: IPv4Range,
    occupiedIps :: [IPv4],
    dns :: [IPv4]
  }

data DConfiguration = DConfiguration
  { serverIp :: Text,
    gateway :: Text,
    network :: Text,
    occupiedIps :: [Text],
    dns :: [Text]
  }
  deriving (Show, Generic, FromDhall)

maybeP :: (Text -> Maybe a) -> Text -> Either String a
maybeP f t = case f t of
  Nothing -> Left $ "Cannot parse: " <> unpack t
  Just x -> Right x

actualConfiguration :: DConfiguration -> Either String Configuration
actualConfiguration DConfiguration {..} =
  Configuration
    <$> ipP serverIp
    <*> ipP gateway
    <*> maybeP decodeRange network
    <*> traverse ipP occupiedIps
    <*> traverse ipP dns
  where
    ipP = maybeP decode

readConfig :: IO (Either String Configuration)
readConfig = do
  exists <- doesFileExist "config.dhall"
  if exists
    then do
      txt <- TIO.readFile "config.dhall"
      actualConfiguration <$> input auto txt
    else pure $ Left "config file does not exist"
