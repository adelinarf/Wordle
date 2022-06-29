{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Cabal_example (
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

bindir     = "/home/runner/HideousUnimportantSearchengine/.cabal-sandbox/bin"
libdir     = "/home/runner/HideousUnimportantSearchengine/.cabal-sandbox/lib/x86_64-linux-ghc-8.6.5/Cabal-example-0.1.0.0-AflLez6b2dPHqNGMnJnMgE"
dynlibdir  = "/home/runner/HideousUnimportantSearchengine/.cabal-sandbox/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/runner/HideousUnimportantSearchengine/.cabal-sandbox/share/x86_64-linux-ghc-8.6.5/Cabal-example-0.1.0.0"
libexecdir = "/home/runner/HideousUnimportantSearchengine/.cabal-sandbox/libexec/x86_64-linux-ghc-8.6.5/Cabal-example-0.1.0.0"
sysconfdir = "/home/runner/HideousUnimportantSearchengine/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Cabal_example_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Cabal_example_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Cabal_example_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Cabal_example_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Cabal_example_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Cabal_example_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
