--
-- polymorphic eval!
--

import Poly
import System.Plugins.Eval

main = do r <- eval "Fn (\\x y -> x == y)" ["Poly"]
          case r of
               Left e -> putStrLn e
               Right (Fn f) -> do print $ f True True
                                  print $ f 1 2
