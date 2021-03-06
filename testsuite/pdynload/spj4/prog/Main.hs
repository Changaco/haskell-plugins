{-# LANGUAGE ScopedTypeVariables #-}

import System.Plugins
import API

src     = "../Plugin.hs"
wrap    = "../Wrapper.hs"
apipath = "../api"

main = do status <- make src ["-i"++apipath]
          case status of
                MakeSuccess _ _ -> f
                MakeFailure es  -> mapM_ putStrLn es >> error "there was a type error"

  where f = do v <- pdynload "../Plugin.o" ["../api"] [] "API.Interface" "resource"
               case v of
                 LoadSuccess _ (a :: Interface) -> print $ field a -- will crash
                 LoadFailure es  -> mapM_ putStrLn es
