{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_reader (
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

bindir     = "/Users/carterweinberg/haskellPractice/reader/.stack-work/install/x86_64-osx/d2d2e12fb37ab806b25cc2e499fa69740138fd6c5f8730f5f147a7459925a0d4/8.8.4/bin"
libdir     = "/Users/carterweinberg/haskellPractice/reader/.stack-work/install/x86_64-osx/d2d2e12fb37ab806b25cc2e499fa69740138fd6c5f8730f5f147a7459925a0d4/8.8.4/lib/x86_64-osx-ghc-8.8.4/reader-0.1.0.0-CDh1fMPGpFeKviCOPb5DYT-reader"
dynlibdir  = "/Users/carterweinberg/haskellPractice/reader/.stack-work/install/x86_64-osx/d2d2e12fb37ab806b25cc2e499fa69740138fd6c5f8730f5f147a7459925a0d4/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/carterweinberg/haskellPractice/reader/.stack-work/install/x86_64-osx/d2d2e12fb37ab806b25cc2e499fa69740138fd6c5f8730f5f147a7459925a0d4/8.8.4/share/x86_64-osx-ghc-8.8.4/reader-0.1.0.0"
libexecdir = "/Users/carterweinberg/haskellPractice/reader/.stack-work/install/x86_64-osx/d2d2e12fb37ab806b25cc2e499fa69740138fd6c5f8730f5f147a7459925a0d4/8.8.4/libexec/x86_64-osx-ghc-8.8.4/reader-0.1.0.0"
sysconfdir = "/Users/carterweinberg/haskellPractice/reader/.stack-work/install/x86_64-osx/d2d2e12fb37ab806b25cc2e499fa69740138fd6c5f8730f5f147a7459925a0d4/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "reader_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "reader_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "reader_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "reader_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "reader_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "reader_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
