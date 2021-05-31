{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_invariant (
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
version = Version [0,5,3] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/minh/.cabal/store/ghc-8.6.5/invariant-0.5.3-575579898534876b51043be64f701e01e087fa03c005dae4ed46ec56f6e05591/bin"
libdir     = "/home/minh/.cabal/store/ghc-8.6.5/invariant-0.5.3-575579898534876b51043be64f701e01e087fa03c005dae4ed46ec56f6e05591/lib"
dynlibdir  = "/home/minh/.cabal/store/ghc-8.6.5/invariant-0.5.3-575579898534876b51043be64f701e01e087fa03c005dae4ed46ec56f6e05591/lib"
datadir    = "/home/minh/.cabal/store/ghc-8.6.5/invariant-0.5.3-575579898534876b51043be64f701e01e087fa03c005dae4ed46ec56f6e05591/share"
libexecdir = "/home/minh/.cabal/store/ghc-8.6.5/invariant-0.5.3-575579898534876b51043be64f701e01e087fa03c005dae4ed46ec56f6e05591/libexec"
sysconfdir = "/home/minh/.cabal/store/ghc-8.6.5/invariant-0.5.3-575579898534876b51043be64f701e01e087fa03c005dae4ed46ec56f6e05591/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "invariant_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "invariant_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "invariant_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "invariant_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "invariant_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "invariant_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
