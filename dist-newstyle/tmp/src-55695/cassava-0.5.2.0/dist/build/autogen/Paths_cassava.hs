{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cassava (
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
version = Version [0,5,2,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/minh/.cabal/store/ghc-8.6.5/cassava-0.5.2.0-b08ee7f9667a222f884dfa7511081c8ff97074cbc0d613d259ab26af29621a1e/bin"
libdir     = "/home/minh/.cabal/store/ghc-8.6.5/cassava-0.5.2.0-b08ee7f9667a222f884dfa7511081c8ff97074cbc0d613d259ab26af29621a1e/lib"
dynlibdir  = "/home/minh/.cabal/store/ghc-8.6.5/cassava-0.5.2.0-b08ee7f9667a222f884dfa7511081c8ff97074cbc0d613d259ab26af29621a1e/lib"
datadir    = "/home/minh/.cabal/store/ghc-8.6.5/cassava-0.5.2.0-b08ee7f9667a222f884dfa7511081c8ff97074cbc0d613d259ab26af29621a1e/share"
libexecdir = "/home/minh/.cabal/store/ghc-8.6.5/cassava-0.5.2.0-b08ee7f9667a222f884dfa7511081c8ff97074cbc0d613d259ab26af29621a1e/libexec"
sysconfdir = "/home/minh/.cabal/store/ghc-8.6.5/cassava-0.5.2.0-b08ee7f9667a222f884dfa7511081c8ff97074cbc0d613d259ab26af29621a1e/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cassava_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cassava_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cassava_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cassava_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cassava_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cassava_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
