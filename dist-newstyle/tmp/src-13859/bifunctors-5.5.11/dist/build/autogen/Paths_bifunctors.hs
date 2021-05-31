{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_bifunctors (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [5,5,11] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/minh/.cabal/store/ghc-8.6.5/bifunctors-5.5.11-0bf8f63a13137c883db79273431421bd4fa6bbc5824c5cc7eff3a1ea2bd9fe35/bin"
libdir     = "/home/minh/.cabal/store/ghc-8.6.5/bifunctors-5.5.11-0bf8f63a13137c883db79273431421bd4fa6bbc5824c5cc7eff3a1ea2bd9fe35/lib"
dynlibdir  = "/home/minh/.cabal/store/ghc-8.6.5/bifunctors-5.5.11-0bf8f63a13137c883db79273431421bd4fa6bbc5824c5cc7eff3a1ea2bd9fe35/lib"
datadir    = "/home/minh/.cabal/store/ghc-8.6.5/bifunctors-5.5.11-0bf8f63a13137c883db79273431421bd4fa6bbc5824c5cc7eff3a1ea2bd9fe35/share"
libexecdir = "/home/minh/.cabal/store/ghc-8.6.5/bifunctors-5.5.11-0bf8f63a13137c883db79273431421bd4fa6bbc5824c5cc7eff3a1ea2bd9fe35/libexec"
sysconfdir = "/home/minh/.cabal/store/ghc-8.6.5/bifunctors-5.5.11-0bf8f63a13137c883db79273431421bd4fa6bbc5824c5cc7eff3a1ea2bd9fe35/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bifunctors_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bifunctors_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "bifunctors_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "bifunctors_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bifunctors_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bifunctors_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
