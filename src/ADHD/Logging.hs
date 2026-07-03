module ADHD.Logging where

import System.IO (hFlush, hPutStrLn, stderr)

-- | Log level
data Level = Info | Error

red :: String -> String
red text = "\ESC[31m" ++ text ++ "\ESC[0m"

-- | Log text with specified level
log :: Level -> String -> IO ()
log level str = do
  hPutStrLn stderr text
  hFlush stderr
  where
    text = case level of
      Error -> red $ "[ERROR]: " <> str
      Info -> "[INFO]: " <> str
