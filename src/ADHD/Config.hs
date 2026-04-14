module ADHD.Config where

import Net.IPv4

data Configuration = Configuration
  { serverIp :: IPv4,
    gateway :: IPv4,
    netMask :: IPv4,
    dns :: [IPv4]
  }
