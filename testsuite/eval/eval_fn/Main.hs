--
-- lambda abstraction!
--
--
-- needs unsafeEval because eval has a broken Dynamic check
--
import System.Plugins.Eval

main = do r <- unsafeEval "(\\x -> (x,x::Int))" [] :: IO (Either String (Int -> (Int,Int)))
          either putStrLn (print . ($ 7)) r
