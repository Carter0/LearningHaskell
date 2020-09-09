{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_applicative (
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

bindir     = "/Users/carterweinberg/haskellPractice/applicative/.stack-work/install/x86_64-osx/76839e35383357f80318d55bac54457c6c14baa3a11728968a9431a53e842fad/8.8.3/bin"
libdir     = "/Users/carterweinberg/haskellPractice/applicative/.stack-work/install/x86_64-osx/76839e35383357f80318d55bac54457c6c14baa3a11728968a9431a53e842fad/8.8.3/lib/x86_64-osx-ghc-8.8.3/applicative-0.1.0.0-I0ooKzosZnlBJUhQRCHK4I-applicative"
dynlibdir  = "/Users/carterweinberg/haskellPractice/applicative/.stack-work/install/x86_64-osx/76839e35383357f80318d55bac54457c6c14baa3a11728968a9431a53e842fad/8.8.3/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/carterweinberg/haskellPractice/applicative/.stack-work/install/x86_64-osx/76839e35383357f80318d55bac54457c6c14baa3a11728968a9431a53e842fad/8.8.3/share/x86_64-osx-ghc-8.8.3/applicative-0.1.0.0"
libexecdir = "/Users/carterweinberg/haskellPractice/applicative/.stack-work/install/x86_64-osx/76839e35383357f80318d55bac54457c6c14baa3a11728968a9431a53e842fad/8.8.3/libexec/x86_64-osx-ghc-8.8.3/applicative-0.1.0.0"
sysconfdir = "/Users/carterweinberg/haskellPractice/applicative/.stack-work/install/x86_64-osx/76839e35383357f80318d55bac54457c6c14baa3a11728968a9431a53e842fad/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "applicative_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "applicative_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "applicative_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "applicative_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "applicative_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "applicative_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
