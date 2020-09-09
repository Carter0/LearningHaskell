{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_monad (
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

bindir     = "/Users/carterweinberg/haskellPractice/monad/.stack-work/install/x86_64-osx/4d19bb268085e9443f99e8800faeff7226eefe49b4d60edc52c33ab41f9959e6/8.8.3/bin"
libdir     = "/Users/carterweinberg/haskellPractice/monad/.stack-work/install/x86_64-osx/4d19bb268085e9443f99e8800faeff7226eefe49b4d60edc52c33ab41f9959e6/8.8.3/lib/x86_64-osx-ghc-8.8.3/monad-0.1.0.0-DCXs2Gx8ulxB14U3bKsGWe-monad"
dynlibdir  = "/Users/carterweinberg/haskellPractice/monad/.stack-work/install/x86_64-osx/4d19bb268085e9443f99e8800faeff7226eefe49b4d60edc52c33ab41f9959e6/8.8.3/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/carterweinberg/haskellPractice/monad/.stack-work/install/x86_64-osx/4d19bb268085e9443f99e8800faeff7226eefe49b4d60edc52c33ab41f9959e6/8.8.3/share/x86_64-osx-ghc-8.8.3/monad-0.1.0.0"
libexecdir = "/Users/carterweinberg/haskellPractice/monad/.stack-work/install/x86_64-osx/4d19bb268085e9443f99e8800faeff7226eefe49b4d60edc52c33ab41f9959e6/8.8.3/libexec/x86_64-osx-ghc-8.8.3/monad-0.1.0.0"
sysconfdir = "/Users/carterweinberg/haskellPractice/monad/.stack-work/install/x86_64-osx/4d19bb268085e9443f99e8800faeff7226eefe49b4d60edc52c33ab41f9959e6/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "monad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "monad_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "monad_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "monad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monad_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
