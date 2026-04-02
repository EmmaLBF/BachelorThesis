{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_lambdaemma (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/emmalbf/.cabal/bin"
libdir     = "/home/emmalbf/.cabal/lib/x86_64-linux-ghc-8.10.7/lambdaemma-0.1.0.0-inplace-lambdaemma"
dynlibdir  = "/home/emmalbf/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/emmalbf/.cabal/share/x86_64-linux-ghc-8.10.7/lambdaemma-0.1.0.0"
libexecdir = "/home/emmalbf/.cabal/libexec/x86_64-linux-ghc-8.10.7/lambdaemma-0.1.0.0"
sysconfdir = "/home/emmalbf/.cabal/etc"

getBinDir     = catchIO (getEnv "lambdaemma_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "lambdaemma_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "lambdaemma_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "lambdaemma_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lambdaemma_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lambdaemma_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
