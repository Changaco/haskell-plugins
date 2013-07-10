{-# LANGUAGE CPP, ScopedTypeVariables #-}
--
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

module System.Plugins.Utils (
    Arg,
    mkTemp,
    tryExtensions,
    takeBaseName',
    isSublistOf,
    newer,
    handleDoesntExist,
    encode,
    decode,
    EncodedString,
    replace
  ) where

#include "../../../config.h"

import Control.Exception        ( handleJust )
import Data.Char
import Data.List
import Data.Maybe               ( fromMaybe )
import System.Directory         ( doesFileExist, getModificationTime
                                , getTemporaryDirectory )
import System.FilePath          ( replaceExtension, dropExtensions, takeFileName )
import System.IO                ( Handle, openTempFile )
import System.IO.Error          ( ioeGetFileName )

#if __GLASGOW_HASKELL__ >= 604
import System.IO.Error          ( isDoesNotExistError )
#endif


type Arg = String


mkTemp :: IO (FilePath, Handle)
mkTemp = getTemporaryDirectory >>= \tmpd -> openTempFile tmpd "MXXXXX.hs"


tryExtensions :: [String] -> FilePath -> IO (Maybe FilePath)
tryExtensions [] _  = return Nothing
tryExtensions (ext:exts) file = do
    let l = replaceExtension file ext
    b <- doesFileExist l
    if b then return $ Just l
         else tryExtensions exts file


takeBaseName' :: String -> String
takeBaseName' = dropExtensions . takeFileName

------------------------------------------------------------------------

--
-- | Is file1 newer than file2 ?
--
-- If file1 is missing, throws an exception. If file2 is missing, returns True.
--
newer :: FilePath -> FilePath -> IO Bool
newer a b = do
    a_t <- getModificationTime a
    handleDoesntExist (\_ -> return True) $ do
        b_t <- getModificationTime b
        return ( a_t > b_t )

handleDoesntExist :: (FilePath -> IO a) -> IO a -> IO a
handleDoesntExist = handleJust f
    where f e | isDoesNotExistError e = Just $ fromMaybe "" $ ioeGetFileName e
              | otherwise = Nothing

------------------------------------------------------------------------
--
-- | return the Z-Encoding of the string.
--
-- Stolen from GHC. Use -package ghc as soon as possible
--
type EncodedString = String

encode :: String -> EncodedString
encode []     = []
encode (c:cs) = encode_ch c ++ encode cs

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar 'Z' = False
unencodedChar 'z' = False
unencodedChar c   =  c >= 'a' && c <= 'z'
                  || c >= 'A' && c <= 'Z'
                  || c >= '0' && c <= '9'

--
-- Decode is used for user printing.
--
decode :: EncodedString -> String
decode [] = []
decode ('Z' : d : rest) | isDigit d = decode_tuple   d rest
                        | otherwise = decode_upper   d : decode rest
decode ('z' : d : rest) | isDigit d = decode_num_esc d rest
                        | otherwise = decode_lower   d : decode rest
decode (c  : rest) = c : decode rest

decode_upper, decode_lower :: Char -> Char

decode_upper 'L' = '('
decode_upper 'R' = ')'
decode_upper 'M' = '['
decode_upper 'N' = ']'
decode_upper 'C' = ':'
decode_upper 'Z' = 'Z'
decode_upper ch  = error $ "decode_upper can't handle this char `"++[ch]++"'"

decode_lower 'z' = 'z'
decode_lower 'a' = '&'
decode_lower 'b' = '|'
decode_lower 'c' = '^'
decode_lower 'd' = '$'
decode_lower 'e' = '='
decode_lower 'g' = '>'
decode_lower 'h' = '#'
decode_lower 'i' = '.'
decode_lower 'l' = '<'
decode_lower 'm' = '-'
decode_lower 'n' = '!'
decode_lower 'p' = '+'
decode_lower 'q' = '\''
decode_lower 'r' = '\\'
decode_lower 's' = '/'
decode_lower 't' = '*'
decode_lower 'u' = '_'
decode_lower 'v' = '%'
decode_lower ch  = error $ "decode_lower can't handle this char `"++[ch]++"'"

-- Characters not having a specific code are coded as z224U
decode_num_esc :: Char -> [Char] -> String
decode_num_esc d cs
  = go (digitToInt d) cs
  where
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go n ('U' : rest)           = chr n : decode rest
    go _ other = error $
        "decode_num_esc can't handle this: \""++other++"\""


encode_ch :: Char -> EncodedString
encode_ch c | unencodedChar c = [c]     -- Common case first

-- Constructors
encode_ch '('  = "ZL"   -- Needed for things like (,), and (->)
encode_ch ')'  = "ZR"   -- For symmetry with (
encode_ch '['  = "ZM"
encode_ch ']'  = "ZN"
encode_ch ':'  = "ZC"
encode_ch 'Z'  = "ZZ"

-- Variables
encode_ch 'z'  = "zz"
encode_ch '&'  = "za"
encode_ch '|'  = "zb"
encode_ch '^'  = "zc"
encode_ch '$'  = "zd"
encode_ch '='  = "ze"
encode_ch '>'  = "zg"
encode_ch '#'  = "zh"
encode_ch '.'  = "zi"
encode_ch '<'  = "zl"
encode_ch '-'  = "zm"
encode_ch '!'  = "zn"
encode_ch '+'  = "zp"
encode_ch '\'' = "zq"
encode_ch '\\' = "zr"
encode_ch '/'  = "zs"
encode_ch '*'  = "zt"
encode_ch '_'  = "zu"
encode_ch '%'  = "zv"
encode_ch c    = 'z' : shows (ord c) "U"

decode_tuple :: Char -> EncodedString -> String
decode_tuple d cs
  = go (digitToInt d) cs
  where
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go 0 ['T']          = "()"
    go n ['T']          = '(' : replicate (n-1) ',' ++ ")"
    go 1 ['H']          = "(# #)"
    go n ['H']          = '(' : '#' : replicate (n-1) ',' ++ "#)"
    go _ other = error $ "decode_tuple \'"++other++"'"

-- ---------------------------------------------------------------------

--
-- 'isSublistOf' takes two arguments and returns 'True' iff the first
-- list is a sublist of the second list. This means that the first list
-- is wholly contained within the second list. Both lists must be
-- finite.

isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf [] _ = True
isSublistOf _ [] = False
isSublistOf x y@(_:ys)
    | isPrefixOf x y = True
    | otherwise      = isSublistOf x ys


-- | Replaces all instances of a value in a list by another value.
replace :: Eq a =>
           a   -- ^ Value to look for
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace a b = map (\c -> if c == a then b else c)
