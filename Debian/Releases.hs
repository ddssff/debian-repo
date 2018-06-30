{-# LANGUAGE CPP #-}

-- | Specific information about known Debian-based distributions
-- and releases thereof.
module Debian.Releases
    ( Vendor(..), debian, ubuntu
    , BaseRelease(..)
    -- , baseReleaseDistro
    -- , baseReleaseString
    , allReleases
    , debianReleases
    , ubuntuReleases
    , ReleaseTree(..)
    , Distro(..)
    , releaseName
    , releaseString
    , baseRelease
    , parseReleaseTree
    , isPrivateRelease
    ) where

--import Control.Monad (msum)
--import Data.Char (toLower)
--import Data.Maybe (fromMaybe)
import Data.Set as Set (filter, fromList, Set)
-- The ReleaseName type from the debian library - just a newtyped string.
import Debian.Release (ReleaseName(ReleaseName, relName))

newtype Vendor = Vendor {_unVendor :: String} deriving (Eq, Ord, Show)

data BaseRelease =
    BaseRelease {_vendorName :: Vendor, _releaseName :: ReleaseName}
    deriving (Eq, Ord, Show)

baseReleaseList :: [BaseRelease]
baseReleaseList =
    -- Oldest first
    [ BaseRelease (Vendor "debian") (ReleaseName "sarge") -- 2005/6
    , BaseRelease (Vendor "debian") (ReleaseName "etch") -- 2007/4
    , BaseRelease (Vendor "debian") (ReleaseName "lenny") -- 2009/2
    , BaseRelease (Vendor "debian") (ReleaseName "squeeze") -- 2011/2
    , BaseRelease (Vendor "debian") (ReleaseName "wheezy") -- 2013/5
    , BaseRelease (Vendor "debian") (ReleaseName "jessie") -- 2015/04
    , BaseRelease (Vendor "debian") (ReleaseName "stretch")
    , BaseRelease (Vendor "debian") (ReleaseName "buster")
    , BaseRelease (Vendor "debian") (ReleaseName "bullseye")
    , BaseRelease (Vendor "debian") (ReleaseName "sid") -- always current
    , BaseRelease (Vendor "debian") (ReleaseName "experimental") -- always current (but actually not a base release,
                                          -- not complete.  Is experimental an add-on to sid?)
    -- Ubuntu releases
    , BaseRelease (Vendor "ubuntu") (ReleaseName "dapper") -- 2006/10
    , BaseRelease (Vendor "ubuntu") (ReleaseName "edgy") -- 2007/4
    , BaseRelease (Vendor "ubuntu") (ReleaseName "feisty") -- 2007/10
    , BaseRelease (Vendor "ubuntu") (ReleaseName "hardy") -- 2008/4
    , BaseRelease (Vendor "ubuntu") (ReleaseName "intrepid") -- 2008/10
    , BaseRelease (Vendor "ubuntu") (ReleaseName "jaunty") -- 2009/4
    , BaseRelease (Vendor "ubuntu") (ReleaseName "karmic") -- 2009/10
    , BaseRelease (Vendor "ubuntu") (ReleaseName "lucid") -- 2010/4
    , BaseRelease (Vendor "ubuntu") (ReleaseName "maverick") -- 2010/10
    , BaseRelease (Vendor "ubuntu") (ReleaseName "natty") -- 2011/4
    , BaseRelease (Vendor "ubuntu") (ReleaseName "oneiric") -- 2011/10
    , BaseRelease (Vendor "ubuntu") (ReleaseName "precise") -- 2012/4
    , BaseRelease (Vendor "ubuntu") (ReleaseName "quantal") -- 2012/10
    , BaseRelease (Vendor "ubuntu") (ReleaseName "raring") -- 2013/4
    , BaseRelease (Vendor "ubuntu") (ReleaseName "saucy") -- 2013/10
    , BaseRelease (Vendor "ubuntu") (ReleaseName "trusty") -- 2014/4
    , BaseRelease (Vendor "ubuntu") (ReleaseName "utopic") -- 2014/10
    , BaseRelease (Vendor "ubuntu") (ReleaseName "vivid") -- 2015/04
    , BaseRelease (Vendor "ubuntu") (ReleaseName "wily") -- 2015/10
    , BaseRelease (Vendor "ubuntu") (ReleaseName "xenial") -- 2016/04
    , BaseRelease (Vendor "ubuntu") (ReleaseName "yakkety") -- 2016/10
    , BaseRelease (Vendor "ubuntu") (ReleaseName "zesty") -- 2017/04
    , BaseRelease (Vendor "ubuntu") (ReleaseName "artful") -- 2017/10
    , BaseRelease (Vendor "ubuntu") (ReleaseName "bionic") -- 2018/04
    ]

allReleases :: Set BaseRelease
allReleases = fromList baseReleaseList

debian :: Vendor
debian = Vendor "debian"
ubuntu :: Vendor
ubuntu = Vendor "ubuntu"

debianReleases :: Set BaseRelease
debianReleases = Set.filter ((==) debian . _vendorName) allReleases
ubuntuReleases :: Set BaseRelease
ubuntuReleases = Set.filter ((==) ubuntu . _vendorName) allReleases

newtype Distro = Distro {_distroString :: String} deriving (Eq, Ord, Show)

-- | Interpret the meaning of the release name, e.g. xenial-seereason-private.
data ReleaseTree
    = Foundation BaseRelease
    -- ^ The standalone release which forms the foundation.
    | ExtendedRelease ReleaseTree Distro
    -- ^ A release which is based on another release, known as the
    -- base release.  Thus, this release contains packages which can
    -- be installed on a machine running the base release.
    | PrivateRelease ReleaseTree
    -- ^ A private release based on another release.
    deriving (Eq, Show)

releaseName :: ReleaseTree -> ReleaseName
releaseName = ReleaseName . releaseString

releaseString :: ReleaseTree -> String
releaseString (PrivateRelease r) = releaseString r ++ "-private"
releaseString (ExtendedRelease r vendor) = releaseString r ++ "-" ++ _distroString vendor
releaseString (Foundation name) = relName (_releaseName name)

-- baseReleaseString :: BaseRelease -> String
-- baseReleaseString = _releaseName

baseRelease :: ReleaseTree -> BaseRelease
baseRelease (PrivateRelease release) = baseRelease release
baseRelease (ExtendedRelease release _) = baseRelease release
baseRelease (Foundation release) = release

parseReleaseTree :: ReleaseName -> ReleaseTree
parseReleaseTree (ReleaseName s0) =
    parse s0
    where
      parse "sarge" = Foundation (BaseRelease debian (ReleaseName "sarge"))
      parse "etch" = Foundation (BaseRelease debian (ReleaseName "etch"))
      parse "lenny" = Foundation (BaseRelease debian (ReleaseName "lenny"))
      parse "squeeze" = Foundation (BaseRelease debian (ReleaseName "squeeze"))
      parse "wheezy" = Foundation (BaseRelease debian (ReleaseName "wheezy"))
      parse "jessie" = Foundation (BaseRelease debian (ReleaseName "jessie"))
      parse "sid" = Foundation (BaseRelease debian (ReleaseName "sid"))
      parse "experimental" = Foundation (BaseRelease debian (ReleaseName "experimental"))

      parse "dapper" = Foundation (BaseRelease ubuntu (ReleaseName "dapper"))
      parse "edgy" = Foundation (BaseRelease ubuntu (ReleaseName "edgy"))
      parse "feisty" = Foundation (BaseRelease ubuntu (ReleaseName "feisty"))
      parse "hardy" = Foundation (BaseRelease ubuntu (ReleaseName "hardy"))
      parse "intrepid" = Foundation (BaseRelease ubuntu (ReleaseName "intrepid"))
      parse "jaunty" = Foundation (BaseRelease ubuntu (ReleaseName "jaunty"))
      parse "karmic" = Foundation (BaseRelease ubuntu (ReleaseName "karmic"))
      parse "lucid" = Foundation (BaseRelease ubuntu (ReleaseName "lucid"))
      parse "maverick" = Foundation (BaseRelease ubuntu (ReleaseName "maverick"))
      parse "natty" = Foundation (BaseRelease ubuntu (ReleaseName "natty"))
      parse "oneiric" = Foundation (BaseRelease ubuntu (ReleaseName "oneiric"))
      parse "precise" = Foundation (BaseRelease ubuntu (ReleaseName "precise"))
      parse "quantal" = Foundation (BaseRelease ubuntu (ReleaseName "quantal"))
      parse "raring" = Foundation (BaseRelease ubuntu (ReleaseName "raring"))
      parse "saucy" = Foundation (BaseRelease ubuntu (ReleaseName "saucy"))
      parse "trusty" = Foundation (BaseRelease ubuntu (ReleaseName "trusty"))
      parse "utopic" = Foundation (BaseRelease ubuntu (ReleaseName "utopic"))
      parse "vivid" = Foundation (BaseRelease ubuntu (ReleaseName "vivid"))
      parse "wily" = Foundation (BaseRelease ubuntu (ReleaseName "wily"))
      parse "xenial" = Foundation (BaseRelease ubuntu (ReleaseName "xenial"))
      parse "yakkety" = Foundation (BaseRelease ubuntu (ReleaseName "yakkety"))
      parse "zesty" = Foundation (BaseRelease ubuntu (ReleaseName "zesty"))
      parse "artful" = Foundation (BaseRelease ubuntu (ReleaseName "artful"))
      parse "bionic" = Foundation (BaseRelease ubuntu (ReleaseName "bionic"))

      parse s = case spanEnd (/= '-') s of
                  (_, []) -> error $ "Unknown base release: " ++ show s
                  ([], _) -> error $ "Unknown base release: " ++ show s
                  (base, "private") -> PrivateRelease (parse (init base))
                  (base, distro) -> ExtendedRelease (parse (init base)) (Distro distro)

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p l = (\(a, b) -> (reverse b, reverse a)) $ span p (reverse l)

isPrivateRelease :: ReleaseTree -> Bool
isPrivateRelease (PrivateRelease _) = True
isPrivateRelease _ = False
