{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_QuickCheck (
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
version = Version [2,14,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/minh/.cabal/store/ghc-8.6.5/QuickCheck-2.14.2-efd4df934e2ec43cc0f4257c4399142abd8ea8ac4cdf840eb9a7ec1dd6a60438/bin"
libdir     = "/home/minh/.cabal/store/ghc-8.6.5/QuickCheck-2.14.2-efd4df934e2ec43cc0f4257c4399142abd8ea8ac4cdf840eb9a7ec1dd6a60438/lib"
dynlibdir  = "/home/minh/.cabal/store/ghc-8.6.5/QuickCheck-2.14.2-efd4df934e2ec43cc0f4257c4399142abd8ea8ac4cdf840eb9a7ec1dd6a60438/lib"
datadir    = "/home/minh/.cabal/store/ghc-8.6.5/QuickCheck-2.14.2-efd4df934e2ec43cc0f4257c4399142abd8ea8ac4cdf840eb9a7ec1dd6a60438/share"
libexecdir = "/home/minh/.cabal/store/ghc-8.6.5/QuickCheck-2.14.2-efd4df934e2ec43cc0f4257c4399142abd8ea8ac4cdf840eb9a7ec1dd6a60438/libexec"
sysconfdir = "/home/minh/.cabal/store/ghc-8.6.5/QuickCheck-2.14.2-efd4df934e2ec43cc0f4257c4399142abd8ea8ac4cdf840eb9a7ec1dd6a60438/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "QuickCheck_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "QuickCheck_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "QuickCheck_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "QuickCheck_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "QuickCheck_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "QuickCheck_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
