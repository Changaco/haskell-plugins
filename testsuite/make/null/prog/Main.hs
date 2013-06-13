
-- an example where we want to compile and load a file

import System.Plugins
import API

main = do
    make "../Null.hs" ["-i../api"]
    m_v  <- load "../Null.o" ["../api"] [] "resource"
    v <- case m_v of
        LoadSuccess _ v -> return v
        LoadFailure es  -> mapM_ putStrLn es >> error "load failed"
    print $ a v
