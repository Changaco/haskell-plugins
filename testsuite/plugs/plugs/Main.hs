--
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

import System.Plugins.Eval
import System.Plugins.Load

import System.Exit              ( ExitCode(..), exitWith )
import System.IO
import System.Console.Readline  ( readline, addHistory )

symbol = "resource"

main = do
        putStrLn banner
        putStr "Loading package base" >> hFlush stdout
        loadPackage "base"
        putStrLn " ... linking ..." >> hFlush stdout
        resolveObjs (return ())

        shell []

shell :: [String] -> IO ()
shell imps = do
        s <- readline "plugs> "
        cmd <- case s of
                Nothing          -> exitWith ExitSuccess
                Just (':':'q':_) -> exitWith ExitSuccess
                Just s           -> addHistory s >> return s
        imps' <- run cmd imps
        shell imps'

run :: String -> [String] -> IO [String]
run ""   is = return is
run ":?" is = putStrLn help >> return is

run ":l" _             = return []
run (':':'l':' ':m) is = return (m:is)

run (':':'t':' ':s) is = do
        r <- typeOf s is
        either putStrLn (\ty -> putStrLn $ s ++ " :: " ++ ty) r
        return is

run (':':_) is = putStrLn help >> return is

run s is = do
        r <- unsafeEval ("show $ "++s) is
        either putStrLn putStrLn r
        return is

banner = "\
\           __\n\
\    ____  / /_  ______ ______\n\
\   / __ \\/ / / / / __ `/ ___/     PLugin User's GHCi System, for Haskell 98\n\
\  / /_/ / / /_/ / /_/ (__  )      http://www.cse.unsw.edu.au/~dons/hs-plugins\n\
\ / .___/_/\\__,_/\\__, /____/       Type :? for help\n\
\/_/            /____/\n"

help = "\
\Commands :\n\
\  <expr>               evaluate expression\n\
\  :t <expr>            show type of expression (monomorphic only)\n\
\  :l module            bring module in to scope\n\
\  :l                   clear module list\n\
\  :quit                quit\n\
\  :?                   display this list of commands"
