
import System.Plugins.Make
import System.Plugins.Eval

main = do make "a/Extra.hs" []

          r <- unsafeEval_ "show (Just (1 + 6 :: Int)) ++ extra"
                        ["Data.Maybe", "Extra"]
                        ["-ia"]      -- no make flags
                        []           -- no package.confs
                        ["a"]        -- include paths to load from
                        :: IO (Either String String)

          either putStrLn print r
