--
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | Runplugs: use hs-plugins to run a Haskell expression under
-- controlled conditions.
--
import System.Plugins.Eval      (unsafeEval)

import Data.Char                (chr)
import Control.Monad

import System.Random
import System.Exit              (exitWith, ExitCode(ExitSuccess))
import System.IO                (getContents, putStrLn)
import System.Posix.Resource    (setResourceLimit,
                                 Resource(ResourceCPUTime),
                                 ResourceLimits(ResourceLimits),
                                 ResourceLimit(ResourceLimit))

rlimit = ResourceLimit 3

context = [ "Numeric", "System.Random"
          , "qualified Data.Map as M"
          , "qualified Data.Set as S"
          , "qualified Data.IntSet as I"] ++ datas ++ controls

datas   = map ("Data." ++) [
                "Bits", "Bool", "Char", "Dynamic", "Either",
                "Graph", "Int", "Ix", "List",
                "Maybe", "Ratio", "Tree", "Tuple", "Typeable", "Word"
              ]

controls = map ("Control." ++) ["Monad", "Monad.Reader", "Monad.Fix", "Arrow"]

main = do
    setResourceLimit ResourceCPUTime (ResourceLimits rlimit rlimit)
    s <- getLine
    when (not . null $ s) $ do
        x <- sequence (take 3 (repeat $ getStdRandom (randomR (97,122)) >>= return . chr))
        r <- unsafeEval ("let { "++x++
                         " = \n# 1 \"<irc>\"\n"++s++
                         "\n} in take 2048 (show "++x++
                         ")") context
        either putStrLn putStrLn r
    exitWith ExitSuccess

