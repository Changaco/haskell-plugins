
import System.Plugins.Eval

main = do r <- eval "1 + 6 :: Int" [] :: IO (Either String Int)
          either putStrLn print r
