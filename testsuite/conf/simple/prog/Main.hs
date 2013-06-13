
import System.Plugins
import API

conf    = "../Mailrc.conf"
stub    = "../Mailrc.stub"
apipath = "../api"

main = do
        status <- makeWith conf stub ["-i"++apipath]
        o <- case status of
                MakeFailure e   -> mapM_ putStrLn e >> error "make failed"
                MakeSuccess _ o -> return o
        status <- load o [apipath] [] "resource"
        v <- case status of
                LoadFailure es  -> mapM_ putStrLn es >> error "load failed"
                LoadSuccess _ v -> return v

        user_editor <- editor v
        putStrLn user_editor
        makeCleaner o

