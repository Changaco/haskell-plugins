
import System.Plugins
import API

--
-- what happens if we try to use code that has been unloaded?
--

main = do
    m_v   <- load "../Null.o" ["../api"] [] "resource"
    (m,v) <- case m_v of
        LoadSuccess m v -> return (m,v)
        LoadFailure es  -> mapM_ putStrLn es >> error "load failed"
    print $ a v
    unload m
