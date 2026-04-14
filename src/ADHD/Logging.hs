module ADHD.Logging where

data Level = Info | Error

log :: Level -> String -> IO ()
log Info str = putStrLn $ "[INFO] " <> str
log Error str = putStrLn $ "[ERROR] " <> str
