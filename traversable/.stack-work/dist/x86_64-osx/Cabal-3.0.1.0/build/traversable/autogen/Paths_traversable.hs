{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_traversable (
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

bindir     = "/Users/carterweinberg/haskellPractice/traversable/.stack-work/install/x86_64-osx/bce8be5c41aac954b9456c7f6625c9a5ea490b3bdfeef10a16ec3207c4d627f6/8.8.4/bin"
libdir     = "/Users/carterweinberg/haskellPractice/traversable/.stack-work/install/x86_64-osx/bce8be5c41aac954b9456c7f6625c9a5ea490b3bdfeef10a16ec3207c4d627f6/8.8.4/lib/x86_64-osx-ghc-8.8.4/traversable-0.1.0.0-ICo1m9KS7jbITqjWPlMveF-traversable"
dynlibdir  = "/Users/carterweinberg/haskellPractice/traversable/.stack-work/install/x86_64-osx/bce8be5c41aac954b9456c7f6625c9a5ea490b3bdfeef10a16ec3207c4d627f6/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/carterweinberg/haskellPractice/traversable/.stack-work/install/x86_64-osx/bce8be5c41aac954b9456c7f6625c9a5ea490b3bdfeef10a16ec3207c4d627f6/8.8.4/share/x86_64-osx-ghc-8.8.4/traversable-0.1.0.0"
libexecdir = "/Users/carterweinberg/haskellPractice/traversable/.stack-work/install/x86_64-osx/bce8be5c41aac954b9456c7f6625c9a5ea490b3bdfeef10a16ec3207c4d627f6/8.8.4/libexec/x86_64-osx-ghc-8.8.4/traversable-0.1.0.0"
sysconfdir = "/Users/carterweinberg/haskellPractice/traversable/.stack-work/install/x86_64-osx/bce8be5c41aac954b9456c7f6625c9a5ea490b3bdfeef10a16ec3207c4d627f6/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "traversable_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "traversable_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "traversable_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "traversable_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "traversable_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "traversable_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
