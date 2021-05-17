{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_th_lift (
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
version = Version [0,8,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/minh/.cabal/store/ghc-8.6.5/th-lift-0.8.2-eabe7a006c59fed73287bef905474697e89a9c86c9e6d7b356f4e24340ac8205/bin"
libdir     = "/home/minh/.cabal/store/ghc-8.6.5/th-lift-0.8.2-eabe7a006c59fed73287bef905474697e89a9c86c9e6d7b356f4e24340ac8205/lib"
dynlibdir  = "/home/minh/.cabal/store/ghc-8.6.5/th-lift-0.8.2-eabe7a006c59fed73287bef905474697e89a9c86c9e6d7b356f4e24340ac8205/lib"
datadir    = "/home/minh/.cabal/store/ghc-8.6.5/th-lift-0.8.2-eabe7a006c59fed73287bef905474697e89a9c86c9e6d7b356f4e24340ac8205/share"
libexecdir = "/home/minh/.cabal/store/ghc-8.6.5/th-lift-0.8.2-eabe7a006c59fed73287bef905474697e89a9c86c9e6d7b356f4e24340ac8205/libexec"
sysconfdir = "/home/minh/.cabal/store/ghc-8.6.5/th-lift-0.8.2-eabe7a006c59fed73287bef905474697e89a9c86c9e6d7b356f4e24340ac8205/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "th_lift_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "th_lift_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "th_lift_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "th_lift_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "th_lift_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "th_lift_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
