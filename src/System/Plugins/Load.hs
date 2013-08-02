{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
--
-- Copyright (C) 2004-5 Don Stewart
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

-- | An interface to the GHC runtime's dynamic linker, providing runtime
-- loading and linking of Haskell object files, commonly known as /plugins/.

module System.Plugins.Load (

      -- * The @LoadStatus@ type
      LoadStatus(..)

      -- * High-level interface
      , load
      , load_
      , dynload
      , pdynload
      , pdynload_
      , unload
      , unloadAll
      , reload
      , Module(..)

      -- * Low-level interface
      , initLinker      -- start it up
      , loadFunction    -- retrieve a function from an object
      , loadFunction_   -- retrieve a function from an object
      , loadPackageFunction
      , loadPackage     -- load a ghc library and its cbits
      , unloadPackage   -- unload a ghc library and its cbits
      , loadPackageWith -- load a pkg using the package.conf provided
      , loadShared      -- load a .so object file
      , resolveObjs     -- and resolve symbols

      , loadRawObject   -- load a bare .o. no dep chasing, no .hi file reading

      , Symbol

      , getImports

  ) where

#include "../../../config.h"

import System.Plugins.Env
import System.Plugins.Utils
import System.Plugins.Consts
import System.Plugins.LoadTypes
import System.Plugins.Process

import BinIface
import HscTypes
import Module (moduleName, moduleNameString, packageIdString)
import HscMain (newHscEnv)
import TcRnMonad (initTcRnIf)

import Data.Dynamic          ( fromDynamic, Dynamic )
import Data.Typeable         ( Typeable )

import Control.Monad            ( when, forM, forM_ )
import System.Directory         ( findFile, removeFile )
import System.FilePath          ( dropExtension, replaceExtension, takeDirectory )
import Foreign.C.String         ( CString, withCString, peekCString )

#if !MIN_VERSION_ghc(7,2,0)
import GHC                      ( defaultCallbacks )
#else
import DynFlags                 (defaultDynFlags, initDynFlags)
import GHC.Paths                (libdir)
import SysTools                 (initSysTools)
#endif
import GHC.Ptr                  ( Ptr(..), nullPtr )
#if !MIN_VERSION_ghc(7,4,1)
import GHC.Exts                 ( addrToHValue# )
#else
import GHC.Exts                 ( addrToAny# )
#endif

import GHC.Prim                 ( unsafeCoerce# )

import System.IO                ( hPutStr, hClose )

ifaceModuleName = moduleNameString . moduleName . mi_module

readBinIface' :: FilePath -> IO ModIface
readBinIface' hi_path = do
    -- kludgy as hell
#if MIN_VERSION_ghc(7,2,0)
    mySettings <- initSysTools (Just libdir) -- how should we really set the top dir?
    dflags <- initDynFlags (defaultDynFlags mySettings)
    e <- newHscEnv dflags
#else
    e <- newHscEnv defaultCallbacks undefined
#endif
    initTcRnIf 'r' e undefined undefined (readBinIface IgnoreHiWay QuietBinIFaceReading hi_path)

-- TODO need a loadPackage p package.conf :: IO () primitive

--
-- | The @LoadStatus@ type encodes the return status of functions that
-- perform dynamic loading in a type isomorphic to 'Either'. Failure
-- returns a list of error strings, success returns a reference to a
-- loaded module, and the Haskell value corresponding to the symbol that
-- was indexed.
--
data LoadStatus a
        = LoadSuccess Module a
        | LoadFailure Errors

--
-- | 'load' is the basic interface to the dynamic loader. A call to
-- 'load' imports a single object file into the caller's address space,
-- returning the value associated with the symbol requested. Libraries
-- and modules that the requested module depends upon are loaded and
-- linked in turn.
--
-- The first argument is the path to the object file to load, the second
-- argument is a list of directories to search for dependent modules.
-- The third argument is a list of paths to user-defined, but
-- unregistered, /package.conf/ files. The 'Symbol' argument is the
-- symbol name of the value you with to retrieve.
--
-- The value returned must be given an explicit type signature, or
-- provided with appropriate type constraints such that Haskell compiler
-- can determine the expected type returned by 'load', as the return
-- type is notionally polymorphic.
--
-- Example:
--
-- > do mv <- load "Plugin.o" ["api"] [] "resource"
-- >    case mv of
-- >        LoadFailure msg -> print msg
-- >        LoadSuccess _ v -> return v
--
load :: FilePath                -- ^ object file
     -> [FilePath]              -- ^ any include paths
     -> [PackageConf]           -- ^ list of package.conf paths
     -> Symbol                  -- ^ symbol to find
     -> IO (LoadStatus a)

load obj incpaths pkgconfs sym = do
    initLinker

    -- load extra package information
    mapM_ addPkgConf pkgconfs

    -- load the object and its dependencies
    m <- loadRecursive obj incpaths

    -- lookup the requested symbol
    v <- loadFunction m sym
    return $ case v of
        Nothing -> LoadFailure ["load: couldn't find symbol <<"++sym++">>"]
        Just a  -> LoadSuccess m a

--
-- | Like load, but doesn't want a package.conf arg (they are rarely used)
--
load_ :: FilePath    -- ^ object file
      -> [FilePath]  -- ^ any include paths
      -> Symbol      -- ^ symbol to find
      -> IO (LoadStatus a)
load_ o i s = load o i [] s


-- | A work-around for Dynamics. The keys used to compare two TypeReps are
-- somehow not equal for the same type in hs-plugin's loaded objects.
-- Solution: implement our own dynamics...
--
-- The problem with dynload is that it requires the plugin to export
-- a value that is a Dynamic (in our case a (TypeRep,a) pair). If this
-- is not the case, we core dump. Use pdynload if you don't trust the
-- user to supply you with a Dynamic
--
dynload :: Typeable a
        => FilePath
        -> [FilePath]
        -> [PackageConf]
        -> Symbol
        -> IO (LoadStatus a)

dynload obj incpaths pkgconfs sym = do
    s <- load obj incpaths pkgconfs sym
    case s of e@(LoadFailure _)   -> return e
              LoadSuccess m dyn_v -> return $
                    case fromDynamic (unsafeCoerce# dyn_v :: Dynamic) of
                        Just v' -> LoadSuccess m v'
                        Nothing -> LoadFailure ["Mismatched types in interface"]

------------------------------------------------------------------------

-- | The super-replacement for dynload
--
-- Use GHC at runtime so we get staged type inference, providing full
-- power dynamics, *on module interfaces only*. This is quite suitable
-- for plugins, of course :)
--
pdynload :: FilePath                    -- ^ object to load
         -> [FilePath]                  -- ^ include paths
         -> [PackageConf]               -- ^ package confs
         -> Type                        -- ^ API type
         -> Symbol                      -- ^ symbol
         -> IO (LoadStatus a)

pdynload object incpaths pkgconfs ty sym =
    pdynload_ object incpaths pkgconfs [] ty sym

--
-- | Like pdynload, but you can specify extra arguments to the typechecker.
--
pdynload_ :: FilePath       -- ^ object to load
          -> [FilePath]     -- ^ include paths for loading
          -> [PackageConf]  -- ^ any extra package.conf files
          -> [Arg]          -- ^ extra arguments to ghc, when typechecking
          -> Type           -- ^ expected type
          -> Symbol         -- ^ symbol to load
          -> IO (LoadStatus a)

pdynload_ object incpaths pkgconfs args ty sym = do
    (r,err) <- unify object incpaths args ty sym
    case r of
         ExitSuccess -> load object incpaths pkgconfs sym
         ExitFailure _ -> return $ LoadFailure err

------------------------------------------------------------------------
-- | run the typechecker over the constraint file
--
unify obj incs args ty sym = do
        (tmpf,hdl) <- mkTemp
        iface <- readBinIface' $ replaceExtension obj hiSuf

        let src = mkTest (takeBaseName' tmpf) (ifaceModuleName iface) ty sym
            is  = map ("-i"++) incs             -- api
            i   = "-i" ++ takeDirectory obj     -- plugin

        hPutStr hdl src >> hClose hdl

        (r,_,err) <- exec ghc (tmpf:i:is++args++["-fno-code"])
        removeFile tmpf
        return (r,err)

mkTest modnm plugin ty sym =
       "module "++ modnm ++" where" ++
       "\nimport qualified " ++ plugin ++
       "\nimport qualified " ++ (dropExtension ty) ++
       "\n_ = "++ plugin ++"."++ sym ++" :: "++ty

------------------------------------------------------------------------
{-
--
-- a version of load the also unwraps and types a Dynamic object
--
dynload2 :: Typeable a =>
           FilePath ->
           FilePath ->
           Maybe [PackageConf] ->
           Symbol ->
           IO (Module, a)

dynload2 obj incpath pkgconfs sym = do
        (m, v) <- load obj incpath pkgconfs sym
        case fromDynamic v of
            Nothing -> error $ "load: couldn't type "++(show v)
            Just a  -> return (m,a)
-}

------------------------------------------------------------------------
--
-- | unload a module (not its dependencies)
--
-- once you unload it, you can't 'load' it again, you have to 'reload'
-- it. Cause we don't unload all the dependencies
--
unload  :: Module -> IO ()
unload m = rmModuleDeps m >> unloadObj m

------------------------------------------------------------------------
--
-- | unload a module and its dependencies
--
unloadAll :: Module -> IO ()
unloadAll m = do moduleDeps <- getModuleDeps m
                 rmModuleDeps m
                 mapM_ unloadAll moduleDeps
                 unload m


--
-- | Reload a single object file. Don't care about depends, assume they
-- are loaded.
--
-- Assumes you've already done a 'load'.
--
reload :: Module -> Symbol -> IO (LoadStatus a)
reload m sym = do
    unloadObj m >> loadObject m >> resolveObjs (unloadAll m)
    v <- loadFunction m sym
    return $ case v of
                  Nothing -> LoadFailure ["load: couldn't find symbol `"++sym++"'"]
                  Just a  -> LoadSuccess m a


--
-- | Load an object and its dependencies.
--
loadRecursive :: FilePath -> [FilePath] -> IO Module
loadRecursive obj incpaths = do
    -- read the interface file
    let hipath = replaceExtension obj hiSuf
    iface <- handleDoesntExist (\_ -> error $ "File not found: "++hipath)
                               (readBinIface' hipath)
    let deps = mi_deps iface
        (pkgs, mods) = (dep_pkgs deps, dep_mods deps)

    -- filter and load package dependencies
#if MIN_VERSION_ghc(7,2,0)
    pkgs' <- filterLoaded $ map (packageIdString . fst) pkgs
#else
    pkgs' <- filterLoaded $ map packageIdString pkgs
#endif
    mapM_ loadPackage pkgs'
    resolveObjs (mapM_ unloadPackage pkgs')

    -- filter module dependencies
    mods' <- filterLoaded $ map (moduleNameString . fst) mods

    -- find the modules and load them
    moduleDeps <- forM mods' $ \modname -> do
        objpath <- findFile' incpaths $ replace '.' '/' modname ++ objSuf
        loadRecursive objpath incpaths

    -- load and resolve the object
    let m = Module (ifaceModuleName iface) obj False (Just iface)
    loadObject m
    resolveObjs (mapM_ unloadAll (m:moduleDeps))

    -- update the environment
    addModuleDeps m moduleDeps

    return m

  where
    findFile' ds f = maybe (error $ "Couldn't find file: "++f) id
                     `fmap` findFile ds f


--
-- | Load a function from a module (which must be loaded and resolved first).
--

loadFunction :: Module          -- ^ The module the value is in
             -> String          -- ^ Symbol name of value
             -> IO (Maybe a)    -- ^ The value you want
loadFunction (Module{ modKey = k }) = loadFunction_ k

loadFunction_ :: String         -- ^ The key of the module the value is in
              -> String         -- ^ Symbol name of value
              -> IO (Maybe a)   -- ^ The value you want
loadFunction_ = loadFunction__ Nothing

loadFunction__ :: Maybe String
              -> String
              -> String
              -> IO (Maybe a)
loadFunction__ pkg m valsym
   = do let symbol = prefixUnderscore++(maybe "" (\p -> encode p++"_") pkg)
                     ++encode m++"_"++(encode valsym)++"_closure"
        ptr@(Ptr addr) <- withCString symbol c_lookupSymbol
        if (ptr == nullPtr)
            then return Nothing
#if !MIN_VERSION_ghc(7,4,1)
            else case addrToHValue# addr of
#else
            else case addrToAny# addr of
#endif
                (# hval #) -> return ( Just hval )


-- | Loads a function from a package module, given the package name,
--   module name and symbol name.
loadPackageFunction :: String -- ^ Package name, including version number.
                    -> String -- ^ Module name
                    -> String -- ^ Symbol to lookup in the module
                    -> IO (Maybe a)
loadPackageFunction pkgName modName functionName =
    do loadPackage pkgName
       resolveObjs (unloadPackage pkgName)
       loadFunction__ (Just pkgName) modName functionName

--
-- | Load a vanilla or shared object.
--
-- We make it idempotent to stop the nasty problem of loading the same .o twice.
--
loadObject :: Module -> IO ()
loadObject m = isLoaded (modKey m) >>= loadObject' m >> addModule m

loadObject' _ True = return ()

loadObject' (Module{ modIsShared = False, modPath = p }) _ = do
    r <- withCString p c_loadObj
    when (not r) $ error $ "Could not load module `"++p++"'"

loadObject' (Module{ modIsShared = True, modPath = p }) _ = do
    errmsg <- withCString p c_addDLL
    when (errmsg /= nullPtr) $ do
         e <- peekCString errmsg
         error $ "Couldn't load `"++p++"\' because "++e

--
-- | Load a generic .o file, good for loading C objects.
--
loadRawObject :: FilePath -> IO Module
loadRawObject obj = do
    let m = Module (takeBaseName' obj) obj False Nothing
    loadObject m
    return m

--
-- | Resolve (link) the modules loaded by the 'loadObject' function.
--
resolveObjs :: IO a -> IO ()
resolveObjs unloadLoaded
    = do r <- c_resolveObjs
         when (not r) $ unloadLoaded >> error "resolvedObjs failed."


-- | Unload a module
unloadObj :: Module -> IO ()
unloadObj (Module{ modKey = k, modIsShared = shared, modPath = p }) = do
    removed <- rmModule k
    when (removed && not shared) $ do
         r <- withCString p c_unloadObj
         when (not r) $ error "unloadObj: failed"


-- | Load a shared object file (.so or .dll).
loadShared :: FilePath -> IO Module
loadShared obj = do
    let m = Module (takeBaseName' obj) obj True Nothing
    loadObject m
    return m

--
-- | Load a -package that we might need, implicitly loading the cbits too
-- The argument is the name of package (e.g.  \"concurrent\")
--
-- How to find a package is determined by the package.conf info we store
-- in the environment. It is just a matter of looking it up.
--
-- Not printing names of dependent pkgs
--
loadPackage :: String -> IO ()
loadPackage p = do
    (pkgs,objs) <- lookupPkgDeps p
    mapM_ loadPackage pkgs
    mapM_ loadObject objs



--
-- | Unload a package that has already been loaded. Unload the cbits too.
-- The argument is the name of the package.
--
unloadPackage :: String -> IO ()
unloadPackage pkg = do
    (_,mods) <- lookupPkgDeps pkg
    forM_ mods unloadObj

--
-- | load a package using the given package.conf to help
-- TODO should report if it doesn't actually load the package, instead
-- of mapM_ doing nothing like above.
--
loadPackageWith :: String -> [PackageConf] -> IO ()
loadPackageWith p pkgconfs = do
        mapM_ addPkgConf pkgconfs
        loadPackage p


-- ---------------------------------------------------------------------
-- | Nice interface to .hi parser
--
getImports :: String -> IO [String]
getImports m = do
        hi <- readBinIface' (m ++ hiSuf)
        return . map (moduleNameString . fst) . dep_mods . mi_deps $ hi

-- ---------------------------------------------------------------------
-- C interface
--
foreign import ccall safe "lookupSymbol"
   c_lookupSymbol :: CString -> IO (Ptr a)

foreign import ccall unsafe "loadObj"
   c_loadObj :: CString -> IO Bool

foreign import ccall unsafe "unloadObj"
   c_unloadObj :: CString -> IO Bool

foreign import ccall unsafe "resolveObjs"
   c_resolveObjs :: IO Bool

foreign import ccall unsafe "addDLL"
   c_addDLL :: CString -> IO CString

foreign import ccall unsafe "initLinker"
   initLinker :: IO ()
