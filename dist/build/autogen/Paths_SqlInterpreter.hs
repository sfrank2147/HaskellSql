module Paths_SqlInterpreter (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/smf2147/Dropbox/Programming/Haskell/SqlInterpreter/.cabal-sandbox/bin"
libdir     = "/Users/smf2147/Dropbox/Programming/Haskell/SqlInterpreter/.cabal-sandbox/lib/x86_64-osx-ghc-7.8.3/SqlInterpreter-0.1.0.0"
datadir    = "/Users/smf2147/Dropbox/Programming/Haskell/SqlInterpreter/.cabal-sandbox/share/x86_64-osx-ghc-7.8.3/SqlInterpreter-0.1.0.0"
libexecdir = "/Users/smf2147/Dropbox/Programming/Haskell/SqlInterpreter/.cabal-sandbox/libexec"
sysconfdir = "/Users/smf2147/Dropbox/Programming/Haskell/SqlInterpreter/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SqlInterpreter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SqlInterpreter_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "SqlInterpreter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SqlInterpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SqlInterpreter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
