{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_models (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/minh/.cabal/bin"
libdir     = "/home/minh/.cabal/lib/x86_64-linux-ghc-8.6.5/models-0.1.0.0-2ZmP6oGuaNC5urRuSZ0h1c"
dynlibdir  = "/home/minh/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/minh/.cabal/share/x86_64-linux-ghc-8.6.5/models-0.1.0.0"
libexecdir = "/home/minh/.cabal/libexec/x86_64-linux-ghc-8.6.5/models-0.1.0.0"
sysconfdir = "/home/minh/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "models_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "models_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "models_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "models_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "models_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "models_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
