--
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
-- USA
--

--
-- | Evaluate Haskell at runtime, using runtime compilation and dynamic
-- loading. Arguments are compiled to native code, and dynamically
-- loaded, returning a Haskell value representing the compiled argument.
-- The underlying implementation treats 'String' arguments as the source
-- for plugins to be compiled at runtime.
--

module System.Plugins.Eval (
        eval,
        eval_,
        unsafeEval,
        unsafeEval_,
        typeOf,
        mkHsValues,

{-
        hs_eval_b,      -- return a Bool
        hs_eval_c,      -- return a CChar
        hs_eval_i,      -- return a CInt
        hs_eval_s,      -- return a CString
-}

    ) where

import System.Plugins.Load
import System.Plugins.Make
import System.Plugins.Utils

import Control.Applicative
import Data.Char
import Data.Dynamic             ( Dynamic )
import Data.Map as Map
import Data.Typeable            ( Typeable )
import System.Directory
import System.IO                ( hClose, hFlush, hPutStr )
import System.IO.Unsafe
import System.Random

type Import = String

symbol :: Symbol
symbol = "resource"

defaultArgs = ["-O0","-package","plugins"]

-- | 'eval' provides a typesafe (to a limit) form of runtime evaluation
-- for Haskell -- a limited form of /runtime metaprogramming/. The
-- 'String' argument to 'eval' is a Haskell source fragment to evaluate
-- at rutime. @imps@ are a list of module names to use in the context of
-- the compiled value.
--
-- The value returned by 'eval' is constrained to be 'Typeable' --
-- meaning we can perform a /limited/ runtime typecheck, using the
-- 'dynload' function. One consequence of this is that the code must
-- evaluate to a monomorphic value (which will be wrapped in a
-- 'Dynamic').
--
-- If the evaluated code typechecks under the 'Typeable' constraints,
-- 'Right v' is returned. 'Left errors' indicates typechecking failed.
-- Typechecking may fail at two places: when compiling the argument, or
-- when typechecking the splice point. 'eval' resembles a
-- metaprogramming 'run' operator for /closed/ source fragments.
--
-- To evaluate polymorphic values you need to wrap them in data
-- structures using rank-N types.
--
-- Examples:
--
-- > do i <- eval "1 + 6 :: Int" [] :: IO (Either String Int)
-- >    print $ forceEither i
--
eval :: Typeable a => String -> [Import] -> IO (Either String a)
eval src imps = eval_ src imps defaultArgs [] []

--
-- | 'eval_' is a variety of 'eval' with all the internal hooks
-- available. You are able to set any extra arguments to the compiler
-- (for example, optimisation flags) or dynamic loader.
--
eval_ :: Typeable a =>
         String                 -- ^ code to compile
      -> [Import]               -- ^ any imports
      -> [String]               -- ^ extra make flags
      -> [FilePath]             -- ^ (package.confs) for load
      -> [FilePath]             -- ^ include paths load is to search in
      -> IO (Either String a)   -- ^ either an error msg, or a well typed value

eval_ = rawEval dynwrap dynload

-- | Sometimes when constructing string fragments to evaluate, the
-- programmer is able to provide some other constraint on the evaluated
-- string, such that the evaluated expression will be typesafe, without
-- requiring a 'Typeable' constraint. In such cases, the monomorphic
-- restriction is annoying. 'unsafeEval' removes any splice-point
-- typecheck, with an accompanying obligation on the programmer to
-- ensure that the fragment evaluated will be typesafe at the point it
-- is spliced.
--
-- An example of how to do this would be to wrap the fragment in a call
-- to 'show'. The augmented fragment would then be checked when compiled
-- to return a 'String', and the programmer can rely on this, without
-- requiring a splice-point typecheck, and thus no 'Typeable'
-- restriction.
--
-- Note that if you get the proof wrong, your program will likely
-- segfault.
--
-- Example:
--
-- > do s <- unsafeEval "map toUpper \"haskell\"" ["Data.Char"]
-- >    putStrLn $ forceEither s
--
unsafeEval :: String -> [Import] -> IO (Either String a)
unsafeEval src mods = unsafeEval_ src mods defaultArgs [] []

--
-- | 'unsafeEval_' is a form of 'unsafeEval' with all internal hooks
-- exposed. This is useful for application wishing to specify particular
-- libraries to link against and so on.
--
unsafeEval_ :: String           -- ^ code to compile
            -> [Import]         -- ^ any imports
            -> [String]         -- ^ make flags
            -> [FilePath]       -- ^ (package.confs) for load
            -> [FilePath]       -- ^ include paths load is to search in
            -> IO (Either String a)

unsafeEval_ = rawEval wrap load

rawEval wrapper loader src imps args ldflags incs = do
    pwd <- getCurrentDirectory
    (tmpf,tmph) <- mkTemp
    hPutStr tmph $ wrapper src (takeBaseName' tmpf) imps
    hFlush tmph >> hClose tmph
    status <- make tmpf args
    case status of
        MakeFailure es     -> return $ Left $ unlines es
        MakeSuccess _ obj  -> do
            m_v <- loader obj (pwd:incs) ldflags symbol
            case m_v of
                 LoadFailure es     -> return $ Left $ unlines es
                 LoadSuccess _ rsrc -> return $ Right rsrc
      <* makeCleaner tmpf

------------------------------------------------------------------------
--
-- | 'mkHsValues' is a helper function for converting 'Data.Map's
-- of names and values into Haskell code. It relies on the assumption that
-- the passed values' Show instances produce valid Haskell literals
-- (this is true for all Prelude types).
--
mkHsValues :: (Show a) => Map.Map String a -> String
mkHsValues values = concat $ elems $ Map.mapWithKey convertToHs values
        where convertToHs :: (Show a) => String -> a -> String
              convertToHs name value = name ++ " = " ++ show value ++ "\n"

------------------------------------------------------------------------
--
-- | Return a compiled value's type, by using Dynamic to get a
-- representation of the inferred type.
--
typeOf :: String -> [Import] -> IO (Either String String)
typeOf src imps = do
    r <- eval src imps :: IO (Either String Dynamic)
    return $ fmap (init . tail . show) r

-- ---------------------------------------------------------------------
-- wrappers
--
dynwrap :: String -> String -> [Import] -> String
dynwrap expr nm mods =
        "module "++nm++ "( resource ) where\n" ++
         concatMap (\m-> "import "++m++"\n") mods ++
        "import Data.Dynamic\n" ++
        "resource = let { "++x++" = \n" ++
        "{-# LINE 1 \"<eval>\" #-}\n" ++ expr ++ ";} in toDyn "++x
    where
        x = randName ()

wrap :: String -> String -> [Import] -> String
wrap expr nm mods =
        "module "++nm++ "( resource ) where\n" ++
        concatMap (\m-> "import "++m++"\n") mods ++
        "resource = let { "++x++" = \n" ++
        "{-# LINE 1 \"<Plugins.Eval>\" #-}\n" ++ expr ++ ";} in "++x
    where
        x = randName ()

randName () = unsafePerformIO $
    sequence $ replicate 3 $ getStdRandom (randomR (97,122)) >>= return . chr

-- what is this big variable name?
-- its a random value, so that it won't clash if  the accidently mistype
-- an unbound 'x' or 'v' in their code.. it won't reveal the internal
-- structure of the wrapper, which is annoying in irc use by lambdabot

{-
------------------------------------------------------------------------
--
-- And for our friends in foreign parts
--
-- TODO needs to accept char** to import list
--

--
-- return NULL pointer if an error occured.
--

foreign export ccall hs_eval_b  :: CString -> IO (Ptr CInt)
foreign export ccall hs_eval_c  :: CString -> IO (Ptr CChar)
foreign export ccall hs_eval_i  :: CString -> IO (Ptr CInt)
foreign export ccall hs_eval_s  :: CString -> IO CString

------------------------------------------------------------------------
--
-- TODO implement a marshalling for Dynamics, so that we can pass that
-- over to the C side for checking.
--

hs_eval_b :: CString -> IO (Ptr CInt)
hs_eval_b s = do m_v <- eval_cstring s
                 case m_v of Nothing -> return nullPtr
                             Just v  -> new (fromBool v)

hs_eval_c :: CString -> IO (Ptr CChar)
hs_eval_c s = do m_v <- eval_cstring s
                 case m_v of Nothing -> return nullPtr
                             Just v  -> new (castCharToCChar v)

-- should be Integral
hs_eval_i :: CString -> IO (Ptr CInt)
hs_eval_i s = do m_v <- eval_cstring s :: IO (Maybe Int)
                 case m_v of Nothing -> return nullPtr
                             Just v  -> new (fromIntegral v :: CInt)

hs_eval_s :: CString -> IO CString
hs_eval_s s = do m_v <- eval_cstring s
                 case m_v of Nothing -> return nullPtr
                             Just v  -> newCString v

--
-- convenience
--
eval_cstring :: Typeable a => CString -> IO (Maybe a)
eval_cstring cs = do s <- peekCString cs
                     eval s []            -- TODO use eval()

-}
