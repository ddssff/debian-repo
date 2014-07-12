{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module Main where

import Control.Monad.Trans (liftIO)
import Control.OldException (Exception)
import Debian.Control (Control, parseControlFromFile)
import qualified Debian.GenBuildDeps as G
import Debian.Repo.Monad ()
import Debian.Repo.Dependencies
import Debian.Repo.Package (getPackages, binaryPackageSourceVersion)
import Debian.Repo.Types (Arch (Source, Binary),
                          Section (Section),
                          EnvRoot (EnvRoot, rootPath),
                          EnvPath (EnvPath, envRoot, envPath),
                          Layout (Flat),
                          PkgVersion,
                          BinaryPackage,
                          Repository (LocalRepo),
                          ReleaseName (ReleaseName, relName),
                          PackageIndex (PackageIndex, packageIndexRelease, packageIndexComponent, packageIndexArch),
                          Release (Release, releaseRepo, releaseInfo),
                          ReleaseInfo (ReleaseInfo, releaseInfoName, releaseInfoAliases, releaseInfoArchitectures, releaseInfoComponents),
                          LocalRepository (LocalRepository, repoRoot, repoLayout, repoReleaseInfoLocal))
import Extra.TIO (runTIO, defStyle)

-- import Debug.Trace

main :: IO ()
main = {- test1 >> -} test2

test1 :: IO ()
test1 =
    -- Exercise a bug which (used to) print "*** Exception: Codec.Compression.Zlib: premature end of compressed stream"
    runTIO defStyle (getPackages index) >>= either (putStrLn . show) (putStrLn . show . map binaryPackageSourceVersion)
    where
      name = ReleaseName {relName = "jaunty-seereason"}
      info = [ReleaseInfo {releaseInfoName = ReleaseName {relName = "jaunty-seereason"}, releaseInfoAliases = [], releaseInfoArchitectures = [Binary "i386",Binary "amd64"], releaseInfoComponents = [Section "main"]}]
      repo = LocalRepo (LocalRepository {repoRoot = EnvPath {envRoot = EnvRoot {rootPath = ""},
                                                             envPath = "/home/david/.autobuilder/localpools/jaunty-seereason"},
                                         repoLayout = Just Flat,
                                         repoReleaseInfoLocal = info})
      rel = Release {releaseRepo = repo, releaseInfo = ReleaseInfo {releaseInfoName = name, releaseInfoAliases = [], releaseInfoArchitectures = [Binary "i386",Binary "amd64"], releaseInfoComponents = [Section "main"]}}
      index = PackageIndex {packageIndexRelease = rel, packageIndexComponent = Section "main", packageIndexArch = Source}

test2 :: IO ()
test2 =
    do control <- liftIO (parseControlFromFile controlPath) >>= either (error . show) return
       let (_, deps, _) = either (error . show) id (G.buildDependencies control)
       -- packages <- runTIO defStyle (getPackages index) >>= either (error . show) return
       (packages' :: [Either Exception [BinaryPackage]]) <- runTIO defStyle (mapM getPackages indexes)
       let (packages :: [BinaryPackage]) = concatMap (either (error . show) id) packages'
       let sourceVersions = map binaryPackageSourceVersion packages
       let deps' = simplifyRelations packages deps preferred arch
       finish packages deps deps'
    where
      finish packages deps simpledeps =
          putStrLn ({- "package=" ++ show packages ++ "\n\n" ++ -} "deps=" ++ show deps ++ "\n\nsimpledeps=" ++ show simpledeps)
      indexes = [-- makeIndex pooldir "seereason" "karmic-seereason" "main" Source,
                 -- makeIndex pooldir "seereason" "karmic-seereason" "main" (Binary "amd64"),
                 -- makeIndex pooldir "ubuntu" "karmic" "main" Source,
                 -- makeIndex pooldir "ubuntu" "karmic" "main" (Binary "amd64"),
                 makeIndex pooldir "test" "karmic" "main" (Binary "amd64")]
      preferred = []
      arch = Binary "amd64"
      controlPath = pooldir ++ "control"
      pooldir = "./Test/"

makeIndex :: String -> String -> String -> String -> Arch -> PackageIndex
makeIndex pooldir vendor dist section arch =
    PackageIndex {packageIndexRelease = rel, packageIndexComponent = Section section, packageIndexArch = arch}
    where
      rel = Release {releaseRepo = repo,
                     releaseInfo = ReleaseInfo {releaseInfoName = name,
                                                releaseInfoAliases = [],
                                                releaseInfoArchitectures = [Binary "i386",Binary "amd64"],
                                                releaseInfoComponents = [Section "main"]}}
      repo = LocalRepo (LocalRepository {repoRoot = EnvPath {envRoot = EnvRoot {rootPath = ""},
                                                             envPath = pooldir ++ vendor},
                                         repoLayout = Just Flat,
                                         repoReleaseInfoLocal = info})
      info = [ReleaseInfo {releaseInfoName = ReleaseName {relName = dist},
                           releaseInfoAliases = [],
                           releaseInfoArchitectures = [Binary "i386",Binary "amd64"],
                           releaseInfoComponents = [Section section]}]
      name = ReleaseName {relName = dist}

{-
paths =
    ["/var/lib/apt/lists/mirror.anl.gov_pub_ubuntu_dists_karmic_main_binary-amd64_Packages",
     "/var/lib/apt/lists/deb.seereason.com_ubuntu_dists_karmic-seereason_main_binary-amd64_Packages"]
-}
