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
    netMask :: IPv4,
    dns :: [IPv4]
  }

data DConfiguration = DConfiguration
  { serverIp :: Text,
    gateway :: Text,
    netMask :: Text,
    dns :: [Text]
  }
  deriving (Show, Generic, FromDhall)

ipP :: Text -> Either String IPv4
ipP t = case decode t of
  Nothing -> Left $ "Cannot parse IP " <> unpack t
  Just x -> Right x

actualConfiguration :: DConfiguration -> Either String Configuration
actualConfiguration DConfiguration {..} =
  Configuration
    <$> ipP serverIp
    <*> ipP gateway
    <*> ipP netMask
    <*> traverse ipP dns

readConfig :: IO (Either String Configuration)
readConfig = do
  exists <- doesFileExist "config.dhall"
  if exists
    then do
      txt <- TIO.readFile "config.dhall"
      actualConfiguration <$> input auto txt
    else pure $ Left "config file does not exist"
