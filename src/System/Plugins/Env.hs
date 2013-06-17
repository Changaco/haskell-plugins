{-# LANGUAGE CPP #-}
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

module System.Plugins.Env (
        env,
        withModEnv,
        withDepEnv,
        withPkgEnvs,
        withMerged,
        modifyModEnv,
        modifyDepEnv,
        modifyPkgEnv,
        modifyMerged,
        addModule,
        rmModule,
        addModules,
        isLoaded,
        filterLoaded,
        addModuleDeps,
        getModuleDeps,
        rmModuleDeps,
        isMerged,
        lookupMerged,
        addMerge,
        addPkgConf,
        union,
        addStaticPkg,
        isStaticPkg,
        grabDefaultPkgConf,
        readPackageConf,
        lookupPkg

   ) where

#include "../../../config.h"

import System.Plugins.LoadTypes (Module)
import System.Plugins.Consts           ( sysPkgSuffix )

import Control.Monad            ( liftM, filterM )

import Data.IORef               ( writeIORef, readIORef, newIORef, IORef() )
import Data.Maybe               ( isJust, isNothing, fromMaybe )
import Data.List                ( (\\), nub, isSuffixOf )

import System.IO.Unsafe         ( unsafePerformIO )
import System.Directory         ( doesFileExist )
#if defined(CYGWIN) || defined(__MINGW32__)
import System.Win32.Info        ( getSystemDirectory )
#endif

import Control.Concurrent.MVar  ( MVar(), newMVar, withMVar )

import Distribution.Package hiding (depends, packageName, PackageName(..))
import Distribution.Text

import Distribution.InstalledPackageInfo
import Distribution.Simple.Compiler
import Distribution.Simple.GHC
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Verbosity

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

--
-- | We need to record what modules and packages we have loaded, so if
-- we read a .hi file that wants to load something already loaded, we
-- can safely ignore that request. We're in the IO monad anyway, so we
-- can add some extra state of our own.
--
-- The state is a Map String (Module,Int) (a hash of
-- package\/object names to Modules and how many times they've been
-- loaded).
--
-- It also contains the package.conf information, so that if there is a
-- package dependency we can find it correctly, even if it has a
-- non-standard path or name, and if it isn't an official package (but
-- rather one provided via -package-conf). This is stored as a Map
-- PackageName PackageConfig. The problem then is whether a user's
-- package.conf, that uses the same package name as an existing GHC
-- package, should be allowed, or should shadow a library package?  I
-- don't know, but I'm inclined to have the GHC package shadow the
-- user's package.
--
-- This idea is based on /Hampus Ram's dynamic loader/ dependency
-- tracking system. He uses state to record dependency trees to allow
-- clean unloading and other fun. This is quite cool. We're just using
-- state to make sure we don't load the same package twice. Implementing
-- the full dependency tree idea would be nice, though not fully
-- necessary as we have the dependency information store in .hi files,
-- unlike in hram's loader.
--

type ModEnv = Map String (Module,Int)

type DepEnv = Map Module [Module]

-- represents a package.conf file
type PkgEnv  = Map PackageName PackageConfig

type StaticPkgEnv = S.Set PackageName

-- record dependencies between (src,stub) -> merged modid
type MergeEnv = Map (FilePath,FilePath) FilePath

-- multiple package.conf's kept in separate namespaces
type PkgEnvs = [PkgEnv]

data Env = Env {
    envMVar :: MVar (),
    envMod :: IORef ModEnv,
    envDep :: IORef DepEnv,
    envPkg :: IORef PkgEnvs,
    envStaticPkg :: IORef StaticPkgEnv,
    envMerged :: IORef MergeEnv
}


--
-- our environment, contains a set of loaded objects, and a map of known
-- packages and their informations. Initially all we know is the default
-- package.conf information.
--
env = unsafePerformIO $ do
    mvar  <- newMVar ()
    ref1  <- newIORef M.empty         -- loaded objects
    ref2  <- newIORef M.empty
    p     <- grabDefaultPkgConf
    ref3  <- newIORef p               -- package.conf info
    ref4  <- newIORef (S.fromList ["base","Cabal","haskell-src", "containers",
                                    "arrays", "directory", "random", "process",
                                    "ghc", "ghc-prim"])
    ref5  <- newIORef M.empty         -- merged files
    return $ Env mvar ref1 ref2 ref3 ref4 ref5
{-# NOINLINE env #-}

-- -----------------------------------------------------------
--
-- | apply 'f' to the loaded objects Env, apply 'f' to the package.conf
-- Map /locks up the MVar/ so you can't recursively call a function
-- inside a with any -Env function. Nice and threadsafe
--
withModEnv  :: (ModEnv   -> IO a) -> IO a
withDepEnv  :: (DepEnv   -> IO a) -> IO a
withPkgEnvs :: (PkgEnvs  -> IO a) -> IO a
withStaticPkgEnv :: (StaticPkgEnv -> IO a) -> IO a
withMerged  :: (MergeEnv -> IO a) -> IO a

withEnv f = withMVar (envMVar env) $ const f
withModEnv  f = withEnv (readIORef (envMod env) >>= f)
withDepEnv  f = withEnv (readIORef (envDep env) >>= f)
withPkgEnvs f = withEnv (readIORef (envPkg env) >>= f)
withStaticPkgEnv f = withEnv (readIORef (envStaticPkg env) >>= f)
withMerged  f = withEnv (readIORef (envMerged env) >>= f)

-- -----------------------------------------------------------
--
-- write an object name
-- write a new PackageConfig
--
modifyModEnv :: (ModEnv   -> IO ModEnv)  -> IO ()
modifyDepEnv :: (DepEnv   -> IO DepEnv)  -> IO ()
modifyPkgEnv :: (PkgEnvs  -> IO PkgEnvs) -> IO ()
modifyStaticPkgEnv :: (StaticPkgEnv  -> IO StaticPkgEnv) -> IO ()
modifyMerged :: (MergeEnv -> IO MergeEnv)-> IO ()

modifyModEnv f = lockAndWrite (envMod env) f
modifyDepEnv f = lockAndWrite (envDep env) f
modifyPkgEnv f = lockAndWrite (envPkg env) f
modifyStaticPkgEnv f = lockAndWrite (envStaticPkg env) f
modifyMerged f = lockAndWrite (envMerged env) f

lockAndWrite ref f = withEnv (readIORef ref >>= f >>= writeIORef ref)

-- -----------------------------------------------------------
--
-- | insert a loaded module name into the environment
--
addModule :: String -> Module -> IO ()
addModule s m = modifyModEnv $ \e -> let c = maybe 0 snd (M.lookup s e)
                                     in return $ M.insert s (m,c+1) e


--
-- | remove a module name from the environment. Returns True if the
-- module was actually removed.
--
rmModule :: String -> IO Bool
rmModule s = do modifyModEnv $ \e -> let c = maybe 1 snd (M.lookup s e)
                                         e' = M.delete s e
                                     in if c-1 <= 0
                                           then return e'
                                           else return e
                withModEnv $ \e -> return (isNothing  (M.lookup s e))

--
-- | insert a list of module names all in one go
--
addModules :: [(String,Module)] -> IO ()
addModules ns = mapM_ (uncurry addModule) ns

--
-- | is a module\/package already loaded?
--
isLoaded :: String -> IO Bool
isLoaded s = withModEnv $ \e -> return $ isJust (M.lookup s e)

filterLoaded :: [String] -> IO [String]
filterLoaded = filterM (fmap not . isLoaded)

-- -----------------------------------------------------------
--
-- module dependency stuff
--

--
-- | Set the dependencies of a Module.
--
addModuleDeps :: Module -> [Module] -> IO ()
addModuleDeps m deps = modifyDepEnv $ \e -> return $ M.insert m deps e

--
-- | Get module dependencies. Nothing if none have been recored.
--
getModuleDeps :: Module -> IO [Module]
getModuleDeps m = withDepEnv $ \e -> return $ fromMaybe [] (M.lookup m e)


--
-- | Unrecord a module from the environment.
--
rmModuleDeps :: Module -> IO ()
rmModuleDeps m = modifyDepEnv $ \e -> return $ M.delete m e

-- -----------------------------------------------------------
-- Package management stuff

--
-- | Insert a single package.conf (containing multiple configs) means:
-- create a new Map. insert packages into Map. add Map to end of list of Map
-- stored in the environment.
--
addPkgConf :: FilePath -> IO ()
addPkgConf f = do
    ps <- readPackageConf f
    modifyPkgEnv $ \ls -> return $ union ls ps

--
-- | add a new Map for the package.conf to the list of existing ones; if a package occurs multiple
-- times, pick the one with the higher version number as the default (e.g., important for base in
-- GHC 6.12)
--
union :: PkgEnvs -> [PackageConfig] -> PkgEnvs
union ls ps' =
        let e = M.empty -- new Map for this package.conf
        in foldr addOnePkg e ps' : ls
    where
      -- we add each package with and without it's version number and with the full installedPackageId
      addOnePkg p e' = addToPkgEnvs (packageName p) p $
                         addToPkgEnvs (display $ installedPackageId p) p $
                         addToPkgEnvs (display $ sourcePackageId p) p e'

      -- if no version number specified, pick the higher version
      addToPkgEnvs = M.insertWith higherVersion

      higherVersion pkgconf1 pkgconf2
        | installedPackageId pkgconf1 >= installedPackageId pkgconf2 = pkgconf1
        | otherwise                                                  = pkgconf2

--
-- | generate a PkgEnv from the system package.conf
-- The path to the default package.conf was determined by /configure/
-- This imposes a constraint that you must build your plugins with the
-- same ghc you use to build hs-plugins. This is reasonable, we feel.
--

grabDefaultPkgConf :: IO PkgEnvs
grabDefaultPkgConf = do
        pc <- configureAllKnownPrograms silent defaultProgramConfiguration
        pkgIndex <- getInstalledPackages silent [GlobalPackageDB, UserPackageDB] pc
        return $ [] `union` allPackages pkgIndex

--
-- parse a source file, expanding any $libdir we see.
--
readPackageConf :: FilePath -> IO [PackageConfig]
readPackageConf f = do
    pc <- configureAllKnownPrograms silent defaultProgramConfiguration
    pkgIndex <- getInstalledPackages silent [GlobalPackageDB, UserPackageDB, SpecificPackageDB f] pc
    return $ allPackages pkgIndex

-- -----------------------------------------------------------
-- Static package management stuff. A static package is linked with the base
-- application and we should therefore not link with any of the DLLs it requires.

addStaticPkg :: PackageName -> IO ()
addStaticPkg pkg = modifyStaticPkgEnv $ \set -> return $ S.insert pkg set

isStaticPkg :: PackageName -> IO Bool
isStaticPkg pkg = withStaticPkgEnv $ \set -> return $ S.member pkg set

--
-- Package path, given a package name, look it up in the environment and
-- return the path to all the libraries needed to load this package.
--
-- What do we need to load? With the library_dirs as prefix paths:
--      . anything in the hs_libraries fields, libdir expanded
--
--      . anything in the extra_libraries fields (i.e. cbits), expanded,
--
--      which includes system .so files.
--
--      . also load any dependencies now, because of that weird mtl
--      library that lang depends upon, but which doesn't show up in the
--      interfaces for some reason.
--
-- We return all the package paths that possibly exist, and the leave it
-- up to loadObject not to load the same ones twice...
--
lookupPkg :: PackageName -> IO ([FilePath],[FilePath])
lookupPkg pn = go [] pn
    where
      go :: [PackageName] -> PackageName -> IO ([FilePath],[FilePath])
      go seen p = do
        (ps, (f, g)) <- lookupPkg' p
        static <- isStaticPkg p
        (f', g') <- liftM unzip $ mapM (go (nub $ seen ++ ps)) (ps \\ seen)
        return $ (nub $ (concat f') ++ f, if static then [] else nub $ (concat g') ++ g)

data LibrarySpec
   = DLL String         -- -lLib
   | DLLPath FilePath   -- -Lpath

classifyLdInput :: FilePath -> IO (Maybe LibrarySpec)
classifyLdInput ('-':'l':lib) = return (Just (DLL lib))
classifyLdInput ('-':'L':path) = return (Just (DLLPath path))
classifyLdInput _ = return Nothing

-- TODO need to define a MAC\/DARWIN symbol
#if defined(MACOSX)
mkSOName root = "lib" ++ root ++ ".dylib"
#elif defined(CYGWIN) || defined(__MINGW32__)
-- Win32 DLLs have no .dll extension here, because addDLL tries
-- both foo.dll and foo.drv
mkSOName root = root
#else
mkSOName root = "lib" ++ root ++ ".so"
#endif

#if defined(MACOSX)
mkDynPkgName root = mkSOName (root ++ "_dyn")
#else
mkDynPkgName root = mkSOName root
#endif

data HSLib = Static FilePath | Dynamic FilePath

--
-- return any stuff to load for this package, plus the list of packages
-- this package depends on. which includes stuff we have to then load
-- too.
--
lookupPkg' :: PackageName -> IO ([PackageName],([FilePath],[FilePath]))
lookupPkg' p = withPkgEnvs $ \es -> go es p
    where
        go [] _       = return ([],([],[]))
        go (e:es) q = case M.lookup q e of
            Nothing -> go es q     -- look in other pkgs

            Just pkg -> do
                let    hslibs  = hsLibraries pkg
                       extras' = extraLibraries pkg
                       cbits   = filter (isSuffixOf "_cbits") extras'
                       extras  = filter (flip notElem cbits) extras'
                       ldopts  = ldOptions pkg
                       deppkgs = packageDeps pkg
                ldInput <- mapM classifyLdInput ldopts
                let ldOptsLibs  = [ path | Just (DLL path) <- ldInput ]
                    ldOptsPaths = [ path | Just (DLLPath path) <- ldInput ]
                    dlls        = map mkSOName (extras ++ ldOptsLibs)
#if defined(CYGWIN) || defined(__MINGW32__)
                    libdirs = fix_topdir (libraryDirs pkg) ++ ldOptsPaths
#else
                    libdirs = libraryDirs pkg ++ ldOptsPaths
#endif
                -- If we're loading dynamic libs we need the cbits to appear before the
                -- real packages.
                libs <- mapM (findHSlib libdirs) (cbits ++ hslibs)
#if defined(CYGWIN) || defined(__MINGW32__)
                syslibdir <- getSystemDirectory
                libs' <- mapM (findDLL $ syslibdir ++ libdirs) dlls
#else
                libs' <- mapM (findDLL libdirs) dlls
#endif
                let slibs = [ lib | Right (Static lib)  <- libs ]
                    dlibs = [ lib | Right (Dynamic lib) <- libs ]
                return (deppkgs, (slibs,map (either id id) libs' ++ dlibs) )

#if defined(CYGWIN) || defined(__MINGW32__)
        -- replace $topdir
        fix_topdir []        = []
        fix_topdir (x:xs)    = replace_topdir x : fix_topdir xs

        replace_topdir []           = []
        replace_topdir ('$':xs)
            | take 6 xs == "topdir" = ghcLibraryPath ++ (drop 6 xs)
            | otherwise             = '$' : replace_topdir xs
        replace_topdir (x:xs)       = x : replace_topdir xs
#endif
        -- a list elimination form for the Maybe type
        --filterRight :: [Either left right] -> [right]
        --filterRight []           = []
        --filterRight (Right x:xs) = x:filterRight xs
        --filterRight (Left _:xs)  =   filterRight xs

        --
        -- Check that a path to a library actually reaches a library
        findHSlib' :: [FilePath] -> String -> IO (Maybe FilePath)
        findHSlib' [] _           = return Nothing
        findHSlib' (dir:dirs) lib = do
                  let l = dir </> lib
                  b <- doesFileExist l
                  if b then return $ Just l     -- found it!
                       else findHSlib' dirs lib

        findHSslib dirs lib = findHSlib' dirs $ lib ++ sysPkgSuffix
        findHSdlib dirs lib = findHSlib' dirs $ mkDynPkgName lib

        -- Problem: sysPkgSuffix  is ".o", but extra libraries could be
        -- ".so"
        -- Solution: first look for static library, if we don't find it
        -- look for a dynamic version.
        findHSlib :: [FilePath] -> String -> IO (Either String HSLib)
        findHSlib dirs lib = do
            static <- findHSslib dirs lib
            case static of
                Just file -> return $ Right $ Static file
                Nothing	  -> do
                    dynamic <- findHSdlib dirs lib
                    case dynamic of
                        Just file -> return $ Right $ Dynamic file
                        Nothing	  -> return $ Left lib

        findDLL :: [FilePath] -> String -> IO (Either String FilePath)
        findDLL [] lib         = return (Left lib)
        findDLL (dir:dirs) lib = do
                 let l = dir </> lib
                 b <- doesFileExist l
                 if b then return $ Right l
                      else findDLL dirs lib

------------------------------------------------------------------------
-- do we have a Module name for this merge?
--
isMerged :: FilePath -> FilePath -> IO Bool
isMerged a b = withMerged $ \e -> return $ isJust (M.lookup (a,b) e)

lookupMerged :: FilePath -> FilePath -> IO (Maybe FilePath)
lookupMerged a b = withMerged $ \e -> return $ M.lookup (a,b) e

--
-- insert a new merge pair into env
--
addMerge :: FilePath -> FilePath -> FilePath -> IO ()
addMerge a b z = modifyMerged $ \e -> return $ M.insert (a,b) z e

------------------------------------------------------------------------
-- break a module cycle
-- private:
--
(</>) :: FilePath -> FilePath -> FilePath
[] </> b = b
a  </> b = a ++ "/" ++ b


------------------------------------------------------------------------

--
-- We export an abstract interface to package conf`s because we have
-- to handle either traditional or Cabal style package conf`s.
--



packageName    :: PackageConfig -> PackageName
packageDeps    :: PackageConfig -> [PackageName]
-- updImportDirs  :: ([FilePath] -> [FilePath]) -> PackageConfig -> PackageConfig
-- updLibraryDirs :: ([FilePath] -> [FilePath]) -> PackageConfig -> PackageConfig


type PackageName = String

type PackageConfig = InstalledPackageInfo

packageName = display . pkgName . sourcePackageId
-- packageName_ = pkgName . sourcePackageId
packageDeps = (map display) . depends

{-
updImportDirs f pk@(InstalledPackageInfo { importDirs = idirs }) =
        pk { importDirs = f idirs }
updLibraryDirs f pk@(InstalledPackageInfo { libraryDirs = ldirs }) =
        pk { libraryDirs = f ldirs }
-}
