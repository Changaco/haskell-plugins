
import System.Plugins

import System.Directory

a    = "Foo.hs" -- uesr code
b    = "Bar.hs" -- trusted code. Result is "Bar.o"
c    = "Out.hs"

main = do
        status <- mergeTo a b c
        f <- case status of
                MergeFailure es    -> mapM_ putStrLn es >> error "mergeto failure"
                MergeSuccess _ _ f -> return f
        print $ f == c

        status <- mergeTo a b c
        f' <- case status of
                MergeFailure es         -> mapM_ putStrLn es >> error "mergeto failure"
                MergeSuccess ReComp _ f -> error "unnec. mergeto"
                MergeSuccess NotReq _ f -> return f -- good, not req

        print $ f == f' && f == c

        status <- make f' []
        o <- case status of
                  MakeFailure es  -> mapM_ putStrLn es >> error "make failed"
                  MakeSuccess _ o -> return o

        m_v   <- load o [] [] "resource"
        v <- case m_v of
            LoadSuccess _ v -> return v
            LoadFailure es  -> mapM_ putStrLn es >> error "load failed"
        print (v :: Int)

        makeCleaner c

