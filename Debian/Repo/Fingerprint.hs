{-# LANGUAGE CPP, DeriveDataTypeable, OverloadedStrings #-}
module Debian.Repo.Fingerprint
    ( RetrieveMethod(..)
    , RetrieveAttribute(..)
    , GitSpec(..)
    , DebSpec(..)
    , Fingerprint(..)
    , readUpstreamFingerprint
    , DownstreamFingerprint(..)
    , packageFingerprint
    , modernizeMethod
    , showFingerprint
    , dependencyChanges
    , showDependencies
    , showDependencies'
    ) where

#if 0
import Control.Applicative ((<$>))
import Control.Monad.State (State, evalState, get, put)
import Data.Char (isSpace)
#endif
import Control.Applicative.Error (maybeRead)
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Generics (everywhere, mkT)
import Data.List as List (intercalate, map)
import Data.Set as Set (Set, toList, toAscList, difference, empty, fromList, map, filter)
import Data.Text (unpack, strip)
import Data.Typeable (Typeable)
import qualified Debian.Control.String as S
import Debian.Pretty (pretty)
import Debian.Relation (BinPkgName(..), SrcPkgName)
import Debian.Repo.Dependencies (readSimpleRelation, showSimpleRelation)
import Debian.Repo.PackageID (PackageID(packageName, packageVersion))
import Debian.Repo.PackageIndex (SourcePackage(sourceParagraph, sourcePackageID))
import Debian.Version (DebianVersion, parseDebianVersion, prettyDebianVersion)
import Extra.Misc(columns)

-- | The methods we know for obtaining source code.
data RetrieveMethod
    = Apt String String                      -- ^ Apt dist name - download using apt-get (FIXME: Apt String SrcPkgName would be better, but that breaks read/show)
    | Bzr String                             -- ^ Download from a Bazaar repository
    | Cd FilePath RetrieveMethod             -- ^ Get the source code from a subdirectory of another download
    | Darcs String                           -- ^ Download from a Darcs repository
    | DataFiles RetrieveMethod RetrieveMethod FilePath
                                             -- ^ The first tree is a cabal package, copy the files in the second tree into
                                             -- the first at the location specified by FilePath.  Typically you would then patch
                                             -- the cabal file to add entries to the Data-Files list.
    | DebDir RetrieveMethod RetrieveMethod   -- ^ Combine the upstream download with a download for a debian directory
    | Debianize RetrieveMethod               -- ^ Retrieve a cabal package from Hackage and use cabal-debian to debianize it
    | Debianize' RetrieveMethod [DebSpec]    -- ^ Retrieve a cabal package from Hackage and use cabal-debian to debianize it
    | Dir FilePath                           -- ^ Retrieve the source code from a directory on a local machine
    | Git String [GitSpec]                   -- ^ Download from a Git repository, optional commit hashes and/or branch names
    | Hackage String                         -- ^ Download a cabal package from hackage
    | Hg String                              -- ^ Download from a Mercurial repository
    | Patch RetrieveMethod ByteString        -- ^ Apply the patch given in the string text to the target
    | Proc RetrieveMethod                    -- ^ Mount proc during the build (this should be a PackageFlag.)
    | Quilt RetrieveMethod RetrieveMethod    -- ^ Combine a download with a download of a patch directory to be applied by quilt
    | SourceDeb RetrieveMethod               -- ^ Download and unpack a source deb - a .dsc, a .tar.gz, and a .diff.gz file.
    | Svn String                             -- ^ Download from a Subversion repository
    | Tla String                             -- ^ Download from a TLA repository
    | Twice RetrieveMethod                   -- ^ Perform the build twice (should be a package flag)
    | Uri String String                      -- ^ Download a tarball from the URI.  The checksum is used to implement caching.
    deriving (Read, Show, Eq, Data, Typeable)

-- | If there is some identifying characteristic of the source tree
-- resulting from a retrieve, use a set of RetrieveResult values to
-- identify it so that a build can be triggered if it changes.
-- Examples include the latest Git commit identifier, or a darcs
-- repository tag.  A darcs commit string is not a suitable identifier
-- because darcs commits are not totally ordered, so it can't reliably
-- be used to reconstruct the specific source tree.
data RetrieveAttribute
    = AptVersion DebianVersion
    -- ^ The version number of a package retrieved by apt-get source
    | GitCommit String
    -- ^ The id of the most recent commit
    | DarcsChangesId String
    -- ^ The checksum of the output of darcs changes --xml-output
    deriving (Read, Show, Eq, Ord, Data, Typeable)

data GitSpec
    = Branch String
    | Commit String
    deriving (Read, Show, Eq, Data, Typeable)

data DebSpec
    = SrcDeb SrcPkgName
    deriving (Read, Show, Eq, Data, Typeable)

-- | This type represents a package's fingerprint, (formerly its
-- revision string,) which includes three pieces of information: how
-- it was retrieved, the version number of the resulting Debian source
-- package, and the names and version numbers of the build
-- dependencies against which it was or is about to be built.
data Fingerprint
    = Fingerprint
        { method :: RetrieveMethod
          -- ^ The method which was used to retrieve the source code.
        , retrievedAttributes :: Set RetrieveAttribute
          -- ^ Identifying information about the result of the retrieve,
          -- e.g. the debian version number, darcs repository tag, git
          -- commit id.  Replaces upstreamVersion.
        , upstreamVersion :: DebianVersion
          -- ^ The version number in the changelog of the freshly downloaded
          -- package, before any suffix is added by the autobuilder.
        , buildDependencyVersions :: Set (PackageID BinPkgName)
          -- ^ The names and version numbers of the build dependencies which
          -- were present when the package was build.
        }
    deriving (Show, Eq)

data DownstreamFingerprint
    = DownstreamFingerprint
        { upstreamFingerprint :: Fingerprint
        , downstreamVersion :: DebianVersion
          -- ^ This will be the the version field plus the suffix that
          -- was added by the autobuilder.
        }
    deriving Show

packageFingerprint :: SourcePackage -> Maybe DownstreamFingerprint
packageFingerprint package =
    maybe Nothing readDownstreamFingerprint (fmap (unpack . strip) . S.fieldValue "Fingerprint" . sourceParagraph $ package)
    where
      readDownstreamFingerprint :: String -> Maybe DownstreamFingerprint
      readDownstreamFingerprint s =
          maybe Nothing
                (\ f -> Just $ DownstreamFingerprint { upstreamFingerprint = f
                                                     , downstreamVersion = packageVersion . sourcePackageID $ package })
                (readUpstreamFingerprint s)

readUpstreamFingerprint :: String -> Maybe Fingerprint
readUpstreamFingerprint s =
#if 0
    -- Convert code to use a state monad.  The state will be the
    -- remainder of the string, the result will be the value read.
    evalState readFingerprint s
    where
      readFingerprint :: State String (Maybe Fingerprint)
      readFingerprint = do
        m <- readMethod
        r <- readRetrieveAttributes
        v <- readSourceVersion
        d <- readBuildDeps
        return $ Just $ Fingerprint m r v d
      readString :: State String String
      readString = do
        s <- get
        case reads s of
          [(string, s')] -> put s' >> return string
          _ -> fail $ "readString " ++ show s
      readMethod :: State String RetrieveMethod
      readMethod = do
        s <- readString
        case maybeRead s :: Maybe RetrieveMethod of
          Nothing -> fail $ "readMethod " ++ show s
          Just method -> return method
      readRetrieveAttributes :: State String (Set RetrieveAttribute)
      readRetrieveAttributes = do
        s <- get
        case reads s :: [([RetrieveAttribute], String)] of
          [(attrs, s')] -> put s' >> return $ fromList attrs
          _ -> return empty
      readSourceVersion :: State String DebianVersion
      readSourceVersion = do
        s <- get
        let (v, s') = break isSpace (dropWhile isSpace s)
        put s'
        maybe (fail $ "readSourceVersion " ++ show s) return (parseDebianVersion v)
      readBuildDeps :: State String (Set (PackageID BinPkgName))
      readBuildDeps = do
        rs <- (fromList . List.map readSimpleRelation . words) <$> get
        put ""
        return rs
#else
    case readMethod s of
      Nothing -> Nothing
      Just (m, etc) ->
          let m' = modernizeMethod m in
          -- See if there is a list of RetrieveAttribute - if not use the empty list
          let (attrs, etc') = case reads etc :: [([RetrieveAttribute], String)] of
                                [(x, etc'')] -> (x, etc'')
                                _ -> ([], etc) in
          case words etc' of
            (sourceVersion : buildDeps)
                | not (elem '=' sourceVersion) ->
                    Just $ Fingerprint { method = m'
                                       , upstreamVersion = parseDebianVersion sourceVersion
                                       , retrievedAttributes = Set.fromList attrs
                                       , buildDependencyVersions = fromList (List.map readSimpleRelation buildDeps) }
            -- Old style fingerprint field - no upstream
            -- version number after the method.  I think
            -- at this point we can ignore these.
            _ -> Nothing

readMethod :: String -> Maybe (RetrieveMethod, String)
readMethod s =
    -- New style: read the method directly from the beginning of s
    case reads s :: [(RetrieveMethod, String)] of
      [(m, etc)] -> Just (m, etc)
      -- Old style: read a string, then read the method out of it
      _ -> case reads s :: [(String, String)] of
             [(m, etc)] -> case maybeRead m of
                             Nothing -> Nothing
                             Just m' -> Just (m', etc)
             _ -> Nothing
#endif

modernizeMethod :: RetrieveMethod -> RetrieveMethod
modernizeMethod = everywhere (mkT modernizeMethod1)

modernizeMethod1 :: RetrieveMethod -> RetrieveMethod
modernizeMethod1 (Debianize p) = Debianize' p []
modernizeMethod1 x = x

showFingerprint :: Fingerprint -> String
showFingerprint (Fingerprint {method = m, upstreamVersion = sourceVersion, retrievedAttributes = attrs, buildDependencyVersions = versions}) =
    intercalate " " ["(" ++ show m ++ ")",
                     "[" ++ intercalate ", " (List.map show (toAscList attrs)) ++ "]",
                     show (prettyDebianVersion sourceVersion),
                     intercalate " " (List.map showSimpleRelation (toAscList versions))]

showDependencies :: Fingerprint -> [String]
showDependencies (Fingerprint {buildDependencyVersions = deps}) = toAscList $ Set.map showSimpleRelation deps

-- | Show the dependency list without the version numbers.
showDependencies' :: Fingerprint -> [String]
showDependencies' (Fingerprint {buildDependencyVersions = deps}) = toAscList $ Set.map (show . pretty . packageName) deps

dependencyChanges :: Maybe DownstreamFingerprint -> Fingerprint -> String
dependencyChanges old new =
    depChanges changedDeps
    where
      depChanges [] = ""
      depChanges _ = "  * Build dependency changes:" ++ prefix ++ intercalate prefix padded ++ "\n"
      padded = List.map concat . columns . List.map showDepChange $ changedDeps
      changedDeps = Set.toList (Set.difference (buildDependencyVersions new) (buildDependencyVersions new))
      showDepChange newDep =
          case toList (Set.filter (hasName (packageName newDep)) (maybe empty (buildDependencyVersions . upstreamFingerprint) old)) of
            [] -> [" " ++ unBinPkgName (packageName newDep) ++ ": ", "(none)", " -> ", show (prettyDebianVersion (packageVersion newDep))]
            (oldDep : _) -> [" " ++ unBinPkgName (packageName newDep) ++ ": ", show (prettyDebianVersion (packageVersion oldDep)), " -> ", show (prettyDebianVersion (packageVersion newDep))]
      hasName name = ((== name) . packageName)
      prefix = "\n    "
