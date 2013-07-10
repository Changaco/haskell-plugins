
import System.Plugins

main = do loadPackage "base"
          unloadPackage  "base"
          loadPackage "base"
