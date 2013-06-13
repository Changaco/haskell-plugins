import System.Plugins.Eval

main = do r <- eval "map toUpper \"haskell\"" ["Data.Char"]
          either putStrLn putStrLn r
