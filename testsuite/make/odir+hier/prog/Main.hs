module Main where

import System.Plugins.Make

import System.Directory ( doesFileExist, removeDirectoryRecursive )

main = do
  s <- make "../Sub/Plugin.hs" ["-i../api","-odir","../dist"]
  case s of
    MakeSuccess _ o -> putStrLn o
    MakeFailure e   -> mapM_ putStrLn e
  doesFileExist "../dist/Sub/Plugin.hi" >>= print
  removeDirectoryRecursive "../dist"
  s <- make "../Sub/Plugin.hs" ["-i../api","-hidir","../dist"]
  case s of
    MakeSuccess _ o -> putStrLn o
    MakeFailure e   -> mapM_ putStrLn e
  doesFileExist "../dist/Sub/Plugin.hi" >>= print
  removeDirectoryRecursive "../dist"
  s <- make "../Sub/Plugin.hs" ["-i../api","-odir","../dist","-hidir","../bad"]
  case s of
    MakeSuccess _ o -> putStrLn o
    MakeFailure e   -> mapM_ putStrLn e
  doesFileExist "../dist/Sub/Plugin.hi" >>= print
  removeDirectoryRecursive "../dist"
