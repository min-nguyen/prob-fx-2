{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_aeson (
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
version = Version [1,2,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/minh/.cabal/store/ghc-8.6.5/aeson-1.2.1.0-177a2b3f707563f361f9b5672f114f43b2de3d4a7614368453238f2403be6eb7/bin"
libdir     = "/home/minh/.cabal/store/ghc-8.6.5/aeson-1.2.1.0-177a2b3f707563f361f9b5672f114f43b2de3d4a7614368453238f2403be6eb7/lib"
dynlibdir  = "/home/minh/.cabal/store/ghc-8.6.5/aeson-1.2.1.0-177a2b3f707563f361f9b5672f114f43b2de3d4a7614368453238f2403be6eb7/lib"
datadir    = "/home/minh/.cabal/store/ghc-8.6.5/aeson-1.2.1.0-177a2b3f707563f361f9b5672f114f43b2de3d4a7614368453238f2403be6eb7/share"
libexecdir = "/home/minh/.cabal/store/ghc-8.6.5/aeson-1.2.1.0-177a2b3f707563f361f9b5672f114f43b2de3d4a7614368453238f2403be6eb7/libexec"
sysconfdir = "/home/minh/.cabal/store/ghc-8.6.5/aeson-1.2.1.0-177a2b3f707563f361f9b5672f114f43b2de3d4a7614368453238f2403be6eb7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "aeson_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aeson_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "aeson_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "aeson_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aeson_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aeson_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
