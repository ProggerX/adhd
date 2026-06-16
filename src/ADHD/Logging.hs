module ADHD.Logging where

-- | Log level
data Level = Info | Error

-- | Log text with specified level
log :: Level -> String -> IO ()
log Info str = putStrLn $ "[INFO] " <> str
log Error str = putStrLn $ "[ERROR] " <> str
