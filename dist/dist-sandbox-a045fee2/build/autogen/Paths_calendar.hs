module Paths_calendar (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/maceugene/Functional Programming/calendar/.cabal-sandbox/bin"
libdir     = "/home/maceugene/Functional Programming/calendar/.cabal-sandbox/lib/i386-linux-ghc-7.6.3/calendar-0.1"
datadir    = "/home/maceugene/Functional Programming/calendar/.cabal-sandbox/share/i386-linux-ghc-7.6.3/calendar-0.1"
libexecdir = "/home/maceugene/Functional Programming/calendar/.cabal-sandbox/libexec"
sysconfdir = "/home/maceugene/Functional Programming/calendar/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "calendar_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "calendar_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "calendar_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "calendar_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "calendar_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
