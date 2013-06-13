import System.Plugins
import API

main = do
        m_v <- dynload "../Plugin.o" ["../api"]
                                     []
                                     "resource_dyn"
        case m_v of
                LoadFailure es -> mapM_ putStrLn es >> error "didn't compile"
                LoadSuccess _ (Interface eq) -> do
                                 print $   1 `eq` 2
                                 print $ 'a' `eq` 'b'

