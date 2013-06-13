
import System.Plugins.Eval

main = do i <- eval_ "Just (7 :: Int)"
                     ["Data.Maybe"]
                     []
                     []
                     [] :: IO (Either String (Maybe Int))
          either putStrLn print i
