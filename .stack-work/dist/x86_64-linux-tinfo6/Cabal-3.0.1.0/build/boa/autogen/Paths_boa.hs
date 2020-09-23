{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_boa (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/wimmerth/Documents/ap/week3/code/boa-compiler/.stack-work/install/x86_64-linux-tinfo6/d82df25f6846ff298c3a3c03c372a6701d180aa2f30a207d2905c31b2fe6b43e/8.8.4/bin"
libdir     = "/home/wimmerth/Documents/ap/week3/code/boa-compiler/.stack-work/install/x86_64-linux-tinfo6/d82df25f6846ff298c3a3c03c372a6701d180aa2f30a207d2905c31b2fe6b43e/8.8.4/lib/x86_64-linux-ghc-8.8.4/boa-0.0.0-LF9aTwegbg2HD65dKRD5jA-boa"
dynlibdir  = "/home/wimmerth/Documents/ap/week3/code/boa-compiler/.stack-work/install/x86_64-linux-tinfo6/d82df25f6846ff298c3a3c03c372a6701d180aa2f30a207d2905c31b2fe6b43e/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/wimmerth/Documents/ap/week3/code/boa-compiler/.stack-work/install/x86_64-linux-tinfo6/d82df25f6846ff298c3a3c03c372a6701d180aa2f30a207d2905c31b2fe6b43e/8.8.4/share/x86_64-linux-ghc-8.8.4/boa-0.0.0"
libexecdir = "/home/wimmerth/Documents/ap/week3/code/boa-compiler/.stack-work/install/x86_64-linux-tinfo6/d82df25f6846ff298c3a3c03c372a6701d180aa2f30a207d2905c31b2fe6b43e/8.8.4/libexec/x86_64-linux-ghc-8.8.4/boa-0.0.0"
sysconfdir = "/home/wimmerth/Documents/ap/week3/code/boa-compiler/.stack-work/install/x86_64-linux-tinfo6/d82df25f6846ff298c3a3c03c372a6701d180aa2f30a207d2905c31b2fe6b43e/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "boa_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "boa_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "boa_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "boa_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "boa_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "boa_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
