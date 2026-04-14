module ADHD.Logging where

data Level = Info

log :: Level -> String -> IO ()
log Info str = putStrLn $ "[INFO] " <> str
