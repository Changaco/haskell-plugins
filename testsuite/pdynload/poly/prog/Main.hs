{-# LANGUAGE RankNTypes #-}

import System.Plugins
import API

src     = "../Plugin.hs"
apipath = "../api"

main = do status <- make src ["-i"++apipath]
          case status of
                MakeSuccess _ _ -> f
                MakeFailure e   -> mapM_ putStrLn e

  where f = do v <- pdynload "../Plugin.o" [apipath] [] "API.Interface" "resource"
               case v of
                 LoadSuccess _ a  -> putStrLn "loaded .. yay!"
                 LoadFailure e    -> mapM_ putStrLn e
