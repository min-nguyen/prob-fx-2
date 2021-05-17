{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_reflection (
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
version = Version [2,1,6] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/minh/.cabal/store/ghc-8.6.5/reflection-2.1.6-24c5239dd10e621688f2fa6a62b53d936be72f6eb39917e5c1694f29bd9c72e1/bin"
libdir     = "/home/minh/.cabal/store/ghc-8.6.5/reflection-2.1.6-24c5239dd10e621688f2fa6a62b53d936be72f6eb39917e5c1694f29bd9c72e1/lib"
dynlibdir  = "/home/minh/.cabal/store/ghc-8.6.5/reflection-2.1.6-24c5239dd10e621688f2fa6a62b53d936be72f6eb39917e5c1694f29bd9c72e1/lib"
datadir    = "/home/minh/.cabal/store/ghc-8.6.5/reflection-2.1.6-24c5239dd10e621688f2fa6a62b53d936be72f6eb39917e5c1694f29bd9c72e1/share"
libexecdir = "/home/minh/.cabal/store/ghc-8.6.5/reflection-2.1.6-24c5239dd10e621688f2fa6a62b53d936be72f6eb39917e5c1694f29bd9c72e1/libexec"
sysconfdir = "/home/minh/.cabal/store/ghc-8.6.5/reflection-2.1.6-24c5239dd10e621688f2fa6a62b53d936be72f6eb39917e5c1694f29bd9c72e1/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "reflection_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "reflection_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "reflection_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "reflection_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "reflection_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "reflection_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
