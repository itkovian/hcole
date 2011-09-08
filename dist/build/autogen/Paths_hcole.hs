module Paths_hcole (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/ageorges/Library/Haskell/ghc-7.0.3/lib/hcole-0.0.1/bin"
libdir     = "/Users/ageorges/Library/Haskell/ghc-7.0.3/lib/hcole-0.0.1/lib"
datadir    = "/Users/ageorges/Library/Haskell/ghc-7.0.3/lib/hcole-0.0.1/share"
libexecdir = "/Users/ageorges/Library/Haskell/ghc-7.0.3/lib/hcole-0.0.1/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "hcole_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "hcole_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "hcole_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "hcole_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
