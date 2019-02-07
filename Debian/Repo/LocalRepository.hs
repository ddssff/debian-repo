-- | A repository located on localhost
{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, OverloadedStrings, PackageImports, StandaloneDeriving, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.LocalRepository
    ( LocalRepository(..), repoRoot, repoLayout, repoReleaseInfoLocal
    , Layout(..)
    , poolDir'
    , readLocalRepo
    , copyLocalRepo -- repoCD
    , setRepositoryCompatibility
    , verifyReleaseURI
    , verifyUploadURI
    , uploadRemote
    , uploadLocal
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Applicative.Error (Failing(Success, Failure))
import Control.Exception (IOException)
import Control.Lens (makeLenses, to, view)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (catchError, ExceptT, liftEither, MonadError, runExceptT, withExceptT)
--import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy as L (ByteString, empty)
import Data.List (groupBy, isPrefixOf, partition, sort, sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Set as Set (fromList, member)
import Data.Text as T (unpack)
import Debian.Arch (parseArch)
import Debian.Changes (ChangedFileSpec(changedFileName, changedFileSection), ChangesFile(changeDir, changeFiles, changeInfo, changePackage, changeRelease, changeVersion))
import Debian.Codename (Codename, codename, parseCodename)
import qualified Debian.Control.Text as S (Control'(Control), ControlFunctions(parseControlFromFile), fieldValue)
import qualified Debian.Control.Text as T (fieldValue)
import Debian.Except (HasIOException, liftEIO)
import Debian.Pretty (PP(..))
import Debian.Release (Section(..), sectionName', SubSection(section))
import Debian.Releases (ReleaseURI, vendorFromReleaseURI)
import Debian.Repo.Changes (changeKey, changePath, findChangesFiles)
import Debian.Repo.EnvPath (EnvPath, envPath, outsidePath)
import Debian.Repo.Fingerprint (readUpstreamFingerprint)
--import Debian.Repo.OSKey (HasOSKey)
import Debian.Repo.Prelude (cond, maybeWriteFile, partitionM, replaceFile)
import Debian.Repo.Prelude.Process (runV2)
import Debian.Repo.Prelude.SSH (sshVerify)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Release (parseReleaseFile, Release)
import Debian.Repo.Repo (compatibilityFile, libraryCompatibilityLevel, Repo(..), RepoKey(..))
import Debian.Repo.Rsync (HasRsyncError(fromRsyncError), RsyncError, rsyncOld)
import Debian.TH (here)
import Debian.URI (URI(uriAuthority, uriPath), URIAuth(uriPort, uriRegName, uriUserInfo), uriPathLens, uriToString')
import Debian.VendorURI (VendorURI, vendorURI)
import Debian.Version (parseDebianVersion', prettyDebianVersion)
import Network.URI (URI(..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.Exit (ExitCode, ExitCode(ExitSuccess))
import System.FilePath ((</>), splitFileName)
import qualified System.Posix.Files as F (createLink, getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink, removeLink)
import System.Process (readProcessWithExitCode, CreateProcess(cwd, cmdspec), showCommandForUser, proc)
import System.Process.ListLike (showCmdSpecForUser)
import Text.Regex (matchRegex, mkRegex)
import Text.PrettyPrint.HughesPJClass (text)
import Distribution.Pretty (Pretty(pretty))

data LocalRepository
    = LocalRepository
      { _repoRoot :: EnvPath
      , _repoLayout :: (Maybe Layout)
      , _repoReleaseInfoLocal :: [Release]
      } deriving (Read, Show, Ord, Eq)

-- |The possible file arrangements for a repository.  An empty
-- repository does not yet have either of these attributes.
data Layout = Flat | Pool deriving (Eq, Ord, Read, Show, Bounded, Enum)

$(makeLenses ''LocalRepository)

instance Pretty (PP LocalRepository) where
    pretty (PP (LocalRepository root _ _)) =
        text $ show $ URI { uriScheme = "file:"
                          , uriAuthority = Nothing
                          , uriPath = view envPath root
                          , uriQuery = ""
                          , uriFragment = "" }

instance Repo LocalRepository where
    repoKey (LocalRepository path _ _) = Local path -- fromJust . parseURI $ "file://" ++ envPath path
    repoReleaseInfo (LocalRepository _ _ info) = info

-- | Return the subdirectory where a source package with the given
-- section and name would be installed given the layout of the
-- repository.
poolDir :: LocalRepository -> Section -> String -> FilePath
poolDir r section' source =
    case view repoLayout r of
      Just Pool ->
          "pool/" ++ sectionName' section' </> prefixDir </> source
              where prefixDir =
                        if isPrefixOf "lib" source
                        then take (min 4 (length source)) source
                        else take (min 1 (length source)) source
      _ -> ""

-- | Return the subdirectory in the pool where a source package would be
-- installed.
poolDir' :: LocalRepository -> ChangesFile -> ChangedFileSpec -> FilePath
poolDir' repo changes file =
    case T.fieldValue "Source" (changeInfo changes) of
      Nothing -> error "No 'Source' field in .changes file"
      Just source -> poolDir repo (section . changedFileSection $ file) (unpack source)

readLocalRepo :: MonadIO m => EnvPath -> Maybe Layout -> m (Maybe LocalRepository)
readLocalRepo root layout =
    do names <- liftIO (getDirectoryContents distDir) >>= return . filter (\ x -> not . elem x $ [".", ".."])
       (links, dists) <- partitionM (liftIO . isSymLink . (distDir </>)) names
       linkText <- mapM (liftIO . F.readSymbolicLink) (map (distDir </>) links)
       let aliasPairs = zip linkText links ++ map (\ dist -> (dist, dist)) dists
       let distGroups = groupBy fstEq . sort $ aliasPairs
       let aliases = map (checkAliases  . partition (uncurry (==))) distGroups
       releaseInfo <- mapM (liftIO . getReleaseInfo) aliases
       case releaseInfo of
         [] -> return Nothing
         _ -> return $ Just $ LocalRepository { _repoRoot = root
                                              , _repoLayout = layout
                                              , _repoReleaseInfoLocal = releaseInfo }
    where
      fstEq (a, _) (b, _) = a == b
      checkAliases :: ([(String, String)], [(String, String)]) -> (Codename, [Codename])
      checkAliases ([(realName, _)], aliases) = (parseCodename realName, map (parseCodename . snd) aliases)
      checkAliases _ = error "Symbolic link points to itself!"
      getReleaseInfo :: (Codename, [Codename]) -> IO Release
      getReleaseInfo (dist, aliases) = parseReleaseFile (releasePath dist) dist aliases
      releasePath dist = distDir </> codename dist </> "Release"
      distDir = outsidePath root </> "dists"

isSymLink :: FilePath -> IO Bool
isSymLink path = F.getSymbolicLinkStatus path >>= return . F.isSymbolicLink

-- |Change the root directory of a repository.  FIXME: This should
-- also sync the repository to ensure consistency.
-- repoCD :: EnvPath -> LocalRepository -> LocalRepository
-- repoCD path repo = repo { repoRoot_ = path }

copyLocalRepo ::
    forall e m. (MonadIO m, MonadCatch m,
                 --HasOSKey r, MonadReader r m,
                 HasRsyncError e, HasIOException e, MonadError e m)
    => EnvPath -> LocalRepository -> m LocalRepository
copyLocalRepo dest repo =
    do liftEIO $here $ createDirectoryIfMissing True (outsidePath dest)
       (result :: (Either IOException ExitCode, String, String)) <- action'
       case result of
         (Right ExitSuccess, _, _) -> return $ repo {_repoRoot = dest}
         code -> error $ "*** FAILURE syncing local repository " ++ src ++ " -> " ++ dst ++ ": " ++ show code
    where
      src = outsidePath (view repoRoot repo)
      dst = outsidePath dest
      action' :: m (Either IOException ExitCode, String, String)
      action' = liftEither =<< runExceptT (withExceptT fromRsyncError action)
      action :: forall m'. (MonadIO m', MonadCatch m') => ExceptT RsyncError m' (Either IOException ExitCode, String, String)
      action = rsyncOld [] (outsidePath (view repoRoot repo)) (outsidePath dest)

-- | Create or update the compatibility level file for a repository.
setRepositoryCompatibility :: LocalRepository -> IO ()
setRepositoryCompatibility r =
    maybeWriteFile path (show libraryCompatibilityLevel ++ "\n")
    where path = outsidePath (view repoRoot r) </> compatibilityFile

verifyReleaseURI :: MonadIO m => Bool -> ReleaseURI -> m ()
verifyReleaseURI doExport uri = do
  qPutStrLn ("Verifying release URI: " ++ show uri)
  verifyUploadURI doExport $ vendorFromReleaseURI uri

-- |Make sure we can access the upload uri without typing a password.
verifyUploadURI :: MonadIO m => Bool -> VendorURI -> m ()
verifyUploadURI doExport uri = do
  qPutStrLn ("Verifying vendor URI: " ++ show uri)
  case doExport of
    True -> export
    False -> verify >> mkdir
    where
      export =
          do -- The code in sshExport needs to be rewritten.
             -- liftIO $ uncurry sshExport (uriDest uri) >>= either fail return
             verify
             mkdir
      verify =
          do result <- liftIO $ uncurry sshVerify (uriDest uri)
             case result of
               Right () -> return ()
               Left s -> error $ "Unable to reach " ++ view (vendorURI . to uriToString') uri ++ ": " ++ s
             mkdir
      mkdir =
          case view (vendorURI . to uriAuthority) uri of
            Nothing -> error $ "Internal error 7"
            Just auth ->
                do let cmd = "ssh"
                       args = [uriUserInfo auth ++ uriRegName auth ++ uriPort auth,
                               "mkdir", "-p", view (vendorURI . uriPathLens) uri ++ "/incoming"]
                   (result, _, _) <- liftIO (readProcessWithExitCode cmd args "")
                   case result of
                     ExitSuccess -> return ()
                     _ -> fail $ showCommandForUser cmd args ++ " -> " ++ show result

uriDest :: VendorURI -> ([Char], Maybe Int)
uriDest uri =
    (uriUserInfo auth ++ uriRegName auth, port)
    where
      auth = maybe (error "Internal error 8") id (view (vendorURI . to uriAuthority) uri)
      port =
          case uriPort auth of
            (':' : number) -> Just (read number)
            "" -> Nothing
            x -> error $ "Internal error 9: invalid port " ++ x

-- | Upload all the packages in a local repository to a the incoming
-- directory of a remote repository (using dupload.)
uploadRemote ::
    forall e m. (MonadIO m, MonadCatch m,
                 --HasOSKey r, MonadReader r m,
                 HasIOException e, Show e, MonadError e m)
             => LocalRepository         -- ^ Local repository holding the packages.
             -> VendorURI               -- ^ URI of upload repository
             -> m [Failing (ExitCode, L.ByteString, L.ByteString)]
uploadRemote repo uri =
    do let dir = (outsidePath root)
       changesFiles <- liftIO $ findChangesFiles (outsidePath root)
       let changesFileGroups = map (sortBy compareVersions) . groupByNameAndDist $ changesFiles
       let newestChangesFiles = catMaybes (map listToMaybe changesFileGroups)
       -- hPutStrLn stderr $ "Newest: " ++ show newestChangesFiles
       uploaded <- (Set.fromList .
                    map (\ [_, name', version, arch] -> (name', parseDebianVersion' version, parseArch arch)) .
                    catMaybes .
                    map (matchRegex (mkRegex "^(.*/)?([^_]*)_(.*)_([^.]*)\\.upload$"))) <$> (liftEIO $here (getDirectoryContents dir) :: m [FilePath] {-(MonadError WrappedIOException m' => m' [FilePath])-})
       let (readyChangesFiles, _uploadedChangesFiles) = partition (\ f -> not . Set.member (changeKey f) $ uploaded) newestChangesFiles
       -- hPutStrLn stderr $ "Uploaded: " ++ show uploadedChangesFiles
       -- hPutStrLn stderr $ "Ready: " ++ show readyChangesFiles
       validChangesFiles <- mapM (liftEIO $here . validRevision') readyChangesFiles
       -- hPutStrLn stderr $ "Valid: " ++ show validChangesFiles
       mapM dupload' validChangesFiles
    where
#if 0
      keepNewest (Success newest : older) =
          Success newest : map tooOld older
      keepNewest xs = xs
      -- Add to Control.Applicative.Error
      -- partitionFailing :: [Failing a] -> ([[String]], [a])
      -- partitionFailing = foldr f ([], []) where f (Failure ms) (msgs, xs) = (ms : msgs, xs)
      --                                           f (Success x) (msgs, xs) = (msgs, x : xs)
      tooOld (Failure x) = Failure x
      tooOld (Success x) = Failure ["Not the newest version in incoming: " ++ ppShow x]
      successes (Success x : xs) = x : successes xs
      successes (Failure _ : xs) = successes xs
      successes [] = []
#endif
      root = view repoRoot repo
{-
      rejectOlder :: ([ChangesFile], [(ChangesFile, String)]) ->  ([ChangesFile], [(ChangesFile, String)])
      rejectOlder (accept, reject) =
          (accept', (map tag reject' ++ reject))
          where accept' = map head sortedGroups
                reject' = concat . map tail $ sortedGroups
                sortedGroups = map (sortBy compareVersions) (groupByNameAndDist accept)
                tag x = (x, "Not the newest version in incoming")
-}
      compareVersions a b = compare (changeVersion b) (changeVersion a)
      groupByNameAndDist :: [ChangesFile] -> [[ChangesFile]]
      groupByNameAndDist = groupBy equalNameAndDist . sortBy compareNameAndDist
      equalNameAndDist a b = compareNameAndDist a b == EQ
      compareNameAndDist a b =
          case compare (changePackage a) (changePackage b) of
            EQ -> compare (changeRelease a) (changeRelease b)
            x -> x
      --showReject (changes, tag) = Debian.Repo.Changes.name changes ++ ": " ++ tag
      dupload' :: Failing ChangesFile -> m (Failing (ExitCode, L.ByteString, L.ByteString))
      dupload' (Failure x) = return (Failure x)
      dupload' (Success c) = dupload uri (outsidePath root) (changePath c)

validRevision' :: ChangesFile -> IO (Failing ChangesFile)
validRevision' c = validRevision
    where
      validRevision :: IO (Failing ChangesFile)
      validRevision =
          doesFileExist dscPath >>=
                        cond (S.parseControlFromFile dscPath >>=
                              either (\ e -> return (Failure [show e])) (checkRevision dscPath))
                             (return (Success c))
      dscPath = changeDir c </> changePackage c ++ "_" ++ show (prettyDebianVersion (changeVersion c)) ++ ".dsc"
      checkRevision :: FilePath -> S.Control' String -> IO (Failing ChangesFile)
      checkRevision dscPath' (S.Control [p]) =
          case maybe (Failure ["Missing Fingerprint field in " ++ dscPath'])
                     (\ s -> maybe (Failure ["Parse error in revision string: " ++  show s]) Success (readUpstreamFingerprint s))
                     (S.fieldValue "Fingerprint" p) of
            Failure msgs -> return (Failure msgs)
            -- Success x | x == invalidRevision -> return (Failure ["Invalid revision: " ++ show x])
            Success _ -> return (Success c)
      checkRevision dscPath' _ = return (Failure ["Invalid .dsc file: " ++ show dscPath'])
      -- invalidRevision = "none"
      -- Parse the "Fingerprint:" value describing the origin of the
      -- package's source and the dependency versions used to build it:
      --   Revision: <revisionstring> dep1=ver1 dep2=ver2 ...
      -- parseRevision :: String -> Failing (String, [PackageID BinPkgName])
      -- parseRevision s =
      --     case reads s :: [(String, String)] of
      --       [(method, etc)] ->
      --           case words etc of
      --             (sourceVersion : buildDeps)
      --               | not (elem '=' sourceVersion) ->
      --                   Success (method, map readSimpleRelation buildDeps)
      --             buildDeps ->
      --                   Success (method, map readSimpleRelation buildDeps)
      --       _ -> Failure ["Invalid revision field: " ++ s]

#if 0
-- |The file produced by dupload when a package upload attempt is made.
data UploadFile = Upload FilePath String DebianVersion Arch

uploadKey :: UploadFile -> (String, DebianVersion, Arch)
uploadKey (Upload _ name ver arch) = (name, ver, arch)

-- |Parse the name of a .upload file
parseUploadFilename :: FilePath
                    -> String
                    -> Failing UploadFile
parseUploadFilename dir name =
    case matchRegex (mkRegex "^(.*/)?([^_]*)_(.*)_([^.]*)\\.upload$") name of
      Just [_, name', version, arch] -> Success (Upload dir name' (parseDebianVersion' version) (parseArch arch))
      _ -> Failure ["Invalid .upload file name: " ++ name]
#endif

{-
accept :: (a -> Bool) -> (a -> (a, String)) -> ([a], [(a, String)]) -> ([a], [(a, String)])
accept p tag (accepted, rejected) =
    (accepted', map tag rejected' ++ rejected)
    where (accepted', rejected') = partition p accepted

acceptM :: (Monad m) => (a -> m Bool) -> (a -> (a, String)) -> ([a], [(a, String)]) -> m ([a], [(a, String)])
acceptM p tag (accept, reject) =
    do (accept', reject') <- partitionM p accept
       return (accept', (map tag reject' ++ reject))
-}

-- |Run dupload on a changes file with an optional host (--to)
-- argument.
dupload :: (MonadIO m, MonadCatch m, Show e, MonadError e m)
        => VendorURI
        -> FilePath     -- The directory containing the .changes file
        -> String       -- The name of the .changes file to upload
        -> m (Failing (ExitCode, L.ByteString, L.ByteString))
dupload uri dir changesFile  =
    case view (vendorURI . to uriAuthority) uri of
      Nothing -> error ("Invalid Upload-URI: " ++ show uri)
      Just auth -> do
        let config = ("package config;\n" ++
                      "$cfg{'default'} = {\n" ++
                      "        fqdn => \"" ++ uriRegName auth ++ uriPort auth ++ "\",\n" ++
                      "        method => \"scpb\",\n" ++
                      "        login => \"" ++ init (uriUserInfo auth) ++ "\",\n" ++
                      "        incoming => \"" ++ view (vendorURI . uriPathLens) uri ++ "/incoming\",\n" ++
                      "        dinstall_runs => 1,\n" ++
                      "};\n\n" ++
                      "$preupload{'changes'} = '';\n\n" ++
                      "1;\n")
        liftIO $ replaceFile (dir ++ "/dupload.conf") config
        let cmd = (proc "dupload" ["--to", "default", "-c", (dir ++ "/dupload.conf"), changesFile]) {cwd = Just dir}
        qPutStrLn ("Uploading " ++ show changesFile)
        (runV2 $here cmd L.empty >>= \output -> case output of
                                           (ExitSuccess, _, _) -> return $ Success output
                                           _ -> return $ Failure [show output])
          `catchError` (\(e :: e) -> do
                          let message = "dupload in " ++ dir ++ " failed: " ++ showCmdSpecForUser (cmdspec cmd) ++ " -> " ++
                                        {- either (\ (e :: SomeException) -> show e) (\ (chunks, _elapsed) -> show (collectProcessOutput chunks)) -} show e
                          qPutStrLn message
                          return $ Failure [message])

#if 0
ignore :: forall a. IO (Either String (ExitCode, L.ByteString, L.ByteString)) -> a -> IO (Either String (ExitCode, L.ByteString, L.ByteString))
ignore result _ = result
#endif

-- | Move a build result into a local repository's 'incoming' directory.
uploadLocal :: LocalRepository -> ChangesFile -> IO ()
uploadLocal repo changesFile =
    do let paths = map (\ file -> changeDir changesFile </> changedFileName file) (changeFiles changesFile)
       mapM_ (liftIO . install (outsidePath root)) (changePath changesFile : paths)
    where
      root = view repoRoot repo
      -- Hard link a file into the incoming directory
      install root' path =
          do removeIfExists (dest root' path)
             F.createLink path (dest root' path)
             -- F.removeLink path
      dest root' path = root' ++ "/incoming/" ++ snd (splitFileName path)
      removeIfExists path =
          do exists <- doesFileExist path
             if exists then F.removeLink path  else return ()
