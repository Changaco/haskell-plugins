
import System.Plugins
import API


main = do status <- make "../Plugin.hs" ["-i../api"]
          case status of
                MakeSuccess _ _ -> f
                MakeFailure e   -> mapM_ putStrLn e

   where f = do v <- pdynload "../Plugin.o" ["../api"] [] "API.Interface" "resource"
                case v of
                  LoadSuccess _ a  -> putStrLn "loaded .. yay!"
                  LoadFailure e    -> mapM_ putStrLn e
