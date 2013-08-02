module System.Plugins.Process (exec, ExitCode(..)) where

import System.Exit
import System.Process

--
-- | Slight wrapper over 'readProcessWithExitCode'.
--
exec :: String -> [String] -> IO (ExitCode,[String],[String])
exec cmd args = do
    (r,a,b) <- readProcessWithExitCode cmd args ""
    return (r, lines a, lines b)
