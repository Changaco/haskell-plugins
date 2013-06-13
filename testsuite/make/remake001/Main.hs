
import System.Plugins
import System.Directory

main = do
        status <- make "Foo.hs" [] -- should make
        print status

        status <- make "Foo.hs" [] -- shouldn't make
        print status

        status <- merge "Foo.hs" "Bar.hs"
        case status of
             MergeFailure es     -> error $ unlines es
             MergeSuccess _ _ fp -> do

                 status <- make fp []       -- should make
                 _ <- case status of
                           MakeSuccess c _ -> print c
                           MakeFailure es  -> error $ unlines es

                 status <- make fp []       -- shouldn't make
                 _ <- case status of
                           MakeSuccess c _ -> print c
                           MakeFailure es  -> error $ unlines es
                 removeFile "Foo.o"

