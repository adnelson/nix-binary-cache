module Main where

import Prelude
import System.IO (IO, stdout, stderr, hSetBuffering, BufferMode(LineBuffering))
import Nix.Cache.Client

main :: IO ()
main = do
  -- Make line-buffered, so we can use putStrLn in multithreaded code.
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _ <- loadClientConfig

  putStrLn "Hello world!"
