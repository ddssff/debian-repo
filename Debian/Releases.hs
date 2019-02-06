{-# LANGUAGE CPP, TemplateHaskell #-}

-- | Specific information about known Debian-based distributions
-- and releases thereof.
module Debian.Releases
    (
    -- * URIs
      UploadURI, uploadURI
    , ReleaseURI, releaseURI
    , DistsURI, distsURI
    , vendorFromReleaseURI
    -- * Types
    , BaseVendor(..)
    , baseVendor
    , baseVendorString
    , DebianRelease(..)
    , UbuntuRelease(..)
    , debianReleases
    , ubuntuReleases
    , allReleases
    , ExtendedRelease(..)
    , extendedReleaseVendorName
    , ReleaseTree(..)
    , releaseString
    , parseReleaseTree
    , isPrivateRelease
    -- * Class
    , HasBaseRelease(baseRelease, baseReleaseString) -- new
    -- * Other
{-
    , releaseURI'
    , baseVendorURI
-}
    -- * Tests
    , tests
    , prop_release_string
    ) where

import Control.Lens (makeLenses, over, Prism', prism, review, view)
import Data.Char (toLower)
import Data.List (groupBy)
import Data.Set as Set (fromList, map, Set)
import Debian.Release (ReleaseName(ReleaseName))
import Debian.Sources (VendorURI, vendorURI)
import Debian.URI (nullURI, parentURI, showURI, URI(..), uriPathLens)
import System.FilePath((</>))
import Test.HUnit
import Test.QuickCheck (Arbitrary(arbitrary), elements, oneof)

-- | A value returned by myUploadURI'
newtype UploadURI = UploadURI {_uploadURI :: URI} deriving (Eq, Ord, Show)
-- | A URI ending with /dist/releasename
newtype ReleaseURI = ReleaseURI {_releaseURI :: URI} deriving (Eq, Ord, Show)
-- | The parent of an UploadURI
newtype DistsURI = DistsURI {_distsURI :: URI} deriving (Eq, Ord, Show)

$(makeLenses ''UploadURI)
$(makeLenses ''ReleaseURI)
$(makeLenses ''DistsURI)

vendorFromReleaseURI :: ReleaseURI -> VendorURI
vendorFromReleaseURI = review vendorURI . parentURI . parentURI . view releaseURI

#if 0
class DebianURI u where
  repository :: u -> URI
  -- ^ The URI of the repository, the directory containing dists/,
  -- incoming/, pool/, etc.
  release :: u -> URI
  -- ^ The URI of the repository, a sub-directory of "dists", contains
  -- Release, Release.gpg, and typically main/

-- | A URI with schema ssh: or (for local repositories) file:
newtype DebianUploadURI = DebianUploadURI URI
-- | A URI which may have schema http:, ssh: or file:
newtype DebianDownloadURI = DebianDownloadURI URI

instance DebianURI DebianUploadURI where
    repository (DebianUploadURI uri) = undefined
    release (DebianUploadURI uri) = undefined

instance DebianURI DebianDownloadURI where
    repository (DebianDownloadURI uri) = over uriPathLens (</> (baseVendorPath r)) uri
        where baseVendorPath r = baseVendorName r
    release (DebianDownloadURI uri) = undefined
#endif

-- newtype Vendor = Vendor {_unVendor :: String} deriving (Eq, Ord, Show)

-- | Some Vendors have a Distro.
data BaseVendor = Debian | Ubuntu deriving (Eq, Ord, Show, Bounded, Enum)

data DebianRelease
    = Sarge -- ^ 2005/6
    | Etch -- ^ 2007/4
    | Lenny -- ^ 2009/2
    | Squeeze -- ^ 2011/2
    | Wheezy -- ^ 2013/5
    | Jessie -- ^ 2015/04
    | Stretch
    | Buster
    | Bullseye
    | Sid -- ^ always current
    | Experimental -- ^ always current (but actually not a base release,
                   -- not complete.  Is experimental an add-on to sid?)
    deriving (Eq, Ord, Show, Enum, Bounded)

data UbuntuRelease
    = Dapper -- ^ 2006/10
    | Edgy -- ^ 2007/4
    | Feisty -- ^ 2007/10
    | Hardy -- ^ 2008/4
    | Intrepid -- ^ 2008/10
    | Jaunty -- ^ 2009/4
    | Karmic -- ^ 2009/10
    | Lucid -- ^ 2010/4
    | Maverick -- ^ 2010/10
    | Natty -- ^ 2011/4
    | Oneiric -- ^ 2011/10
    | Precise -- ^ 2012/4
    | Quantal -- ^ 2012/10
    | Raring -- ^ 2013/4
    | Saucy -- ^ 2013/10
    | Trusty -- ^ 2014/4
    | Utopic -- ^ 2014/10
    | Vivid -- ^ 2015/04
    | Wily -- ^ 2015/10
    | Xenial -- ^ 2016/04
    | Yakkety -- ^ 2016/10
    | Zesty -- ^ 2017/04
    | Artful -- ^ 2017/10
    | Bionic -- ^ 2018/04
    deriving (Eq, Ord, Show, Enum, Bounded)

debianReleases :: Set DebianRelease
debianReleases = fromList [minBound..maxBound]
ubuntuReleases :: Set UbuntuRelease
ubuntuReleases = fromList [minBound..maxBound]

allReleases :: Set ReleaseTree
allReleases = Set.map DebianRelease debianReleases <> Set.map UbuntuRelease ubuntuReleases

class HasBaseRelease a where
    baseVendor :: a -> BaseVendor
    baseRelease :: a -> ReleaseTree
    baseReleaseString :: a -> String

instance HasBaseRelease DebianRelease where
    baseVendor _ = Debian
    baseRelease r = DebianRelease r
    baseReleaseString = fmap toLower . show
instance HasBaseRelease UbuntuRelease where
    baseVendor _ = Ubuntu
    baseRelease r = UbuntuRelease r
    baseReleaseString = fmap toLower . show

#if 0
data BaseRelease =
    BaseRelease {_vendor :: BaseVendor, _releaseName :: ReleaseName}
    deriving (Eq, Ord, Show)

baseReleaseList :: [BaseRelease]
baseReleaseList =
    -- Oldest first
    [ BaseRelease Debian (ReleaseName "sarge") -- 2005/6
    , BaseRelease Debian (ReleaseName "etch") -- 2007/4
    , BaseRelease Debian (ReleaseName "lenny") -- 2009/2
    , BaseRelease Debian (ReleaseName "squeeze") -- 2011/2
    , BaseRelease Debian (ReleaseName "wheezy") -- 2013/5
    , BaseRelease Debian (ReleaseName "jessie") -- 2015/04
    , BaseRelease Debian (ReleaseName "stretch")
    , BaseRelease Debian (ReleaseName "buster")
    , BaseRelease Debian (ReleaseName "bullseye")
    , BaseRelease Debian (ReleaseName "sid") -- always current
    , BaseRelease Debian (ReleaseName "experimental") -- always current (but actually not a base release,
                                          -- not complete.  Is experimental an add-on to sid?)
    -- Ubuntu releases
    , BaseRelease Ubuntu (ReleaseName "dapper") -- 2006/10
    , BaseRelease Ubuntu (ReleaseName "edgy") -- 2007/4
    , BaseRelease Ubuntu (ReleaseName "feisty") -- 2007/10
    , BaseRelease Ubuntu (ReleaseName "hardy") -- 2008/4
    , BaseRelease Ubuntu (ReleaseName "intrepid") -- 2008/10
    , BaseRelease Ubuntu (ReleaseName "jaunty") -- 2009/4
    , BaseRelease Ubuntu (ReleaseName "karmic") -- 2009/10
    , BaseRelease Ubuntu (ReleaseName "lucid") -- 2010/4
    , BaseRelease Ubuntu (ReleaseName "maverick") -- 2010/10
    , BaseRelease Ubuntu (ReleaseName "natty") -- 2011/4
    , BaseRelease Ubuntu (ReleaseName "oneiric") -- 2011/10
    , BaseRelease Ubuntu (ReleaseName "precise") -- 2012/4
    , BaseRelease Ubuntu (ReleaseName "quantal") -- 2012/10
    , BaseRelease Ubuntu (ReleaseName "raring") -- 2013/4
    , BaseRelease Ubuntu (ReleaseName "saucy") -- 2013/10
    , BaseRelease Ubuntu (ReleaseName "trusty") -- 2014/4
    , BaseRelease Ubuntu (ReleaseName "utopic") -- 2014/10
    , BaseRelease Ubuntu (ReleaseName "vivid") -- 2015/04
    , BaseRelease Ubuntu (ReleaseName "wily") -- 2015/10
    , BaseRelease Ubuntu (ReleaseName "xenial") -- 2016/04
    , BaseRelease Ubuntu (ReleaseName "yakkety") -- 2016/10
    , BaseRelease Ubuntu (ReleaseName "zesty") -- 2017/04
    , BaseRelease Ubuntu (ReleaseName "artful") -- 2017/10
    , BaseRelease Ubuntu (ReleaseName "bionic") -- 2018/04
    ]
#endif

baseVendorString :: Prism' String BaseVendor
baseVendorString = prism f g
    where
      f Debian = "debian"
      f Ubuntu = "ubuntu"
      g "debian" = Right Debian
      g "ubuntu" = Right Ubuntu
      g s = Left s

data ExtendedRelease = SeeReason84 | SeeReason86 deriving (Eq, Ord, Show, Enum, Bounded)

extendedReleaseVendorName SeeReason84 = "seereason84"
extendedReleaseVendorName SeeReason86 = "seereason86"

-- | Interpret the meaning of the release name, e.g. xenial-seereason-private.
data ReleaseTree
    = PrivateRelease ReleaseTree
    | ExtendedRelease ReleaseTree ExtendedRelease
    | DebianRelease DebianRelease
    | UbuntuRelease UbuntuRelease
    deriving (Eq, Ord, Show)

-- sample (arbitrary :: Gen ReleaseTree)
instance Arbitrary ReleaseTree where
    arbitrary = private
      where
        private = oneof [ PrivateRelease <$> public, public ]
        public = oneof [ ExtendedRelease <$> base <*> arbitrary, base ]
        base = oneof [ DebianRelease <$> arbitrary
                     , UbuntuRelease <$> arbitrary ]

instance Arbitrary ExtendedRelease where arbitrary = elements [minBound .. maxBound]
instance Arbitrary DebianRelease where arbitrary = elements [minBound .. maxBound]
instance Arbitrary UbuntuRelease where arbitrary = elements [minBound .. maxBound]

instance HasBaseRelease ReleaseTree where
    baseVendor (PrivateRelease r) = baseVendor r
    baseVendor (ExtendedRelease r _) = baseVendor r
    baseVendor (DebianRelease _) = Debian
    baseVendor (UbuntuRelease _) = Ubuntu
    baseRelease (PrivateRelease r) = baseRelease r
    baseRelease (ExtendedRelease r _) = baseRelease r
    baseRelease r@(DebianRelease _) = r
    baseRelease r@(UbuntuRelease _) = r
    baseReleaseString (PrivateRelease r) = baseReleaseString r
    baseReleaseString (ExtendedRelease r _) = baseReleaseString r
    baseReleaseString (DebianRelease r) = baseReleaseString r
    baseReleaseString (UbuntuRelease r) = baseReleaseString r

#if 0
data ReleaseTree
    = Foundation BaseRelease
    -- ^ The standalone release which forms the foundation.
    | ExtendedRelease ReleaseTree ExtendedRelease
    -- ^ A release which is some base release extended by another vendor
    -- (such as seereason.)  Thus, this release contains packages which can
    -- be installed on a machine running the base release.
    | PrivateRelease ReleaseTree
    -- ^ A private release based on another release.
    deriving (Eq, Show)
#endif

-- | The string that appears afer the URI in an /etc/apt/sources.list line
releaseString :: ReleaseTree -> String
releaseString (PrivateRelease r) = releaseString r ++ "-private"
releaseString (ExtendedRelease r e) = releaseString r ++ "-" ++ extendedReleaseVendorName e
releaseString (DebianRelease name) = fmap toLower $ show name
releaseString (UbuntuRelease name) = fmap toLower $ show name

-- quickCheck prop_release_string
prop_release_string :: ReleaseTree -> Bool
prop_release_string r =
    parseReleaseTree (ReleaseName $ releaseString r) == r

baseVendorName :: ReleaseTree -> String
baseVendorName (PrivateRelease r) = baseVendorName r
baseVendorName (ExtendedRelease r _e) = baseVendorName r
baseVendorName (DebianRelease _) = "debian"
baseVendorName (UbuntuRelease _) = "ubuntu"

releaseDir :: ReleaseTree -> String
releaseDir (PrivateRelease r) = releaseDir r ++ "-private"
releaseDir (ExtendedRelease r SeeReason84) = releaseDir r ++ "-seereason"
releaseDir (ExtendedRelease r SeeReason86) = releaseDir r ++ "-seereason"
releaseDir (DebianRelease name) = fmap toLower $ show name
releaseDir (UbuntuRelease name) = fmap toLower $ show name

releasePath :: ReleaseTree -> FilePath
releasePath r = baseVendorName r </> "dists" </> releaseDir r

releaseURI' :: ReleaseTree -> URI -> URI
releaseURI' r baseURI = over uriPathLens (</> (releasePath r)) baseURI

#if 0
baseVendorPath :: ReleaseTree -> FilePath
baseVendorPath r = baseVendorName r

baseVendorURI :: ReleaseTree -> URI -> URI
baseVendorURI r baseURI = over uriPathLens (</> (baseVendorPath r)) baseURI
#endif

tests :: Test
tests =
    TestList
    [ TestCase (assertEqual "test1"
                  "URI {uriScheme = \"\", uriAuthority = Nothing, uriPath = \"bionic-seereason\", uriQuery = \"\", uriFragment = \"\"}"
                  (showURI (releaseURI' (ExtendedRelease (UbuntuRelease Bionic) SeeReason84) nullURI)))
    , TestCase (assertEqual "test2"
                  "URI {uriScheme = \"\", uriAuthority = Nothing, uriPath = \"bionic-seereason-private\", uriQuery = \"\", uriFragment = \"\"}"
                  (showURI (releaseURI' (PrivateRelease (ExtendedRelease (UbuntuRelease Bionic) SeeReason84)) nullURI)))
    ]

-- baseReleaseString :: BaseRelease -> String
-- baseReleaseString = _releaseName

parseReleaseTree :: ReleaseName -> ReleaseTree
parseReleaseTree (ReleaseName s0) =
    parse xs0 (UbuntuRelease Bionic)
    where
      xs0 = reverse (Prelude.filter (/= "-") (groupBy (\a b -> (a /= '-') && (b /= '-')) s0))
      parse :: [String] -> ReleaseTree -> ReleaseTree
      parse [] r = r
      parse ("private" : more) r = PrivateRelease (parse more r)
      parse ("seereason84" : more) r = ExtendedRelease (parse more r) SeeReason84
      parse ("seereason86" : more) r = ExtendedRelease (parse more r) SeeReason86

      --parse ("private" : more) = PrivateRelease (parse more)
      -- parse ("seereason84" : more) = ExtendedRelease (parse more) SeeReason84
      -- parse ("seereason86" : more) = ExtendedRelease (parse more) SeeReason86
      -- The base release vendor is implied by the release name
      --parse ("debian" : more) = ExtendedRelease (parse more) Debian
      --parse ("ubuntu" : more) = ExtendedRelease (parse more) Ubuntu

      -- parse xs r = error $ "Unexpected base release name: " ++ show xs ++ " r=" ++ show r ++ " xs0=" ++ show xs0

      parse ("sarge" : more) _ = parse more (DebianRelease Sarge)
      parse ("etch" : more) _ = parse more (DebianRelease Etch)
      parse ("lenny" : more) _ = parse more (DebianRelease Lenny)
      parse ("squeeze" : more) _ = parse more (DebianRelease Squeeze)
      parse ("wheezy" : more) _ = parse more (DebianRelease Wheezy)
      parse ("jessie" : more) _ = parse more (DebianRelease Jessie)
      parse ("stretch" : more) _ = parse more (DebianRelease Stretch)
      parse ("buster" : more) _ = parse more (DebianRelease Buster)
      parse ("bullseye" : more) _ = parse more (DebianRelease Bullseye)
      parse ("sid" : more) _ = parse more (DebianRelease Sid)
      parse ("experimental" : more) _ = parse more (DebianRelease Experimental)

      parse ("dapper" : more) _ = parse more (UbuntuRelease Dapper)
      parse ("edgy" : more) _ = parse more (UbuntuRelease Edgy)
      parse ("feisty" : more) _ = parse more (UbuntuRelease Feisty)
      parse ("hardy" : more) _ = parse more (UbuntuRelease Hardy)
      parse ("intrepid" : more) _ = parse more (UbuntuRelease Intrepid)
      parse ("jaunty" : more) _ = parse more (UbuntuRelease Jaunty)
      parse ("karmic" : more) _ = parse more (UbuntuRelease Karmic)
      parse ("lucid" : more) _ = parse more (UbuntuRelease Lucid)
      parse ("maverick" : more) _ = parse more (UbuntuRelease Maverick)
      parse ("natty" : more) _ = parse more (UbuntuRelease Natty)
      parse ("oneiric" : more) _ = parse more (UbuntuRelease Oneiric)
      parse ("precise" : more) _ = parse more (UbuntuRelease Precise)
      parse ("quantal" : more) _ = parse more (UbuntuRelease Quantal)
      parse ("raring" : more) _ = parse more (UbuntuRelease Raring)
      parse ("saucy" : more) _ = parse more (UbuntuRelease Saucy)
      parse ("trusty" : more) _ = parse more (UbuntuRelease Trusty)
      parse ("utopic" : more) _ = parse more (UbuntuRelease Utopic)
      parse ("vivid" : more) _ = parse more (UbuntuRelease Vivid)
      parse ("wily" : more) _ = parse more (UbuntuRelease Wily)
      parse ("xenial" : more) _ = parse more (UbuntuRelease Xenial)
      parse ("yakkety" : more) _ = parse more (UbuntuRelease Yakkety)
      parse ("zesty" : more) _ = parse more (UbuntuRelease Zesty)
      parse ("artful" : more) _ = parse more (UbuntuRelease Artful)
      parse ("bionic" : more) _ = parse more (UbuntuRelease Bionic)
      parse (s : _more) _ = error ("Unknown base release: " ++ show s ++ " xs0=" ++ show xs0)

isPrivateRelease :: ReleaseTree -> Bool
isPrivateRelease (PrivateRelease _) = True
isPrivateRelease _ = False
