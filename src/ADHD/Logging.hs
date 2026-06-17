module ADHD.Logging where

import System.IO (hFlush, stdout)

-- | Log level
data Level = Info | Error

-- | Log text with specified level
log :: Level -> String -> IO ()
log Info str = putStrLn ("[INFO] " <> str) >> hFlush stdout
log Error str = putStrLn ("[ERROR] " <> str) >> hFlush stdout
