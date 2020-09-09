{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_monoid (
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

bindir     = "/Users/carterweinberg/haskellPractice/monoid/.stack-work/install/x86_64-osx/83e9f62c2238b8e8950dbbb1e0a54156290951865beba37dea03283acb8d604b/8.8.3/bin"
libdir     = "/Users/carterweinberg/haskellPractice/monoid/.stack-work/install/x86_64-osx/83e9f62c2238b8e8950dbbb1e0a54156290951865beba37dea03283acb8d604b/8.8.3/lib/x86_64-osx-ghc-8.8.3/monoid-0.1.0.0-DlAKvyaGcXY1S2GluukWuF-monoid"
dynlibdir  = "/Users/carterweinberg/haskellPractice/monoid/.stack-work/install/x86_64-osx/83e9f62c2238b8e8950dbbb1e0a54156290951865beba37dea03283acb8d604b/8.8.3/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/carterweinberg/haskellPractice/monoid/.stack-work/install/x86_64-osx/83e9f62c2238b8e8950dbbb1e0a54156290951865beba37dea03283acb8d604b/8.8.3/share/x86_64-osx-ghc-8.8.3/monoid-0.1.0.0"
libexecdir = "/Users/carterweinberg/haskellPractice/monoid/.stack-work/install/x86_64-osx/83e9f62c2238b8e8950dbbb1e0a54156290951865beba37dea03283acb8d604b/8.8.3/libexec/x86_64-osx-ghc-8.8.3/monoid-0.1.0.0"
sysconfdir = "/Users/carterweinberg/haskellPractice/monoid/.stack-work/install/x86_64-osx/83e9f62c2238b8e8950dbbb1e0a54156290951865beba37dea03283acb8d604b/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "monoid_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "monoid_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "monoid_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "monoid_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monoid_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monoid_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
