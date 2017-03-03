-- | Launch multi-git-sync server.
module Main
  ( main
  ) where

import Protolude

import MultiGitSync (startApp)

main :: IO ()
main = startApp
