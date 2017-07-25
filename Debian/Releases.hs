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
    , Release(..)
    , Distro(..)
    , releaseName
    , releaseString
    , baseRelease
    , parseReleaseName
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
    -- , BaseRelease (Vendor "debian") (ReleaseName "experimental") -- always current (but actually not a base release,
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

class Distro distro where
    distroString :: distro -> String
    distroParse :: String -> Maybe distro

data Release distro
    = Release BaseRelease
    | ExtendedRelease (Release distro) distro
    -- ^ A release which is based on another release, known as the
    -- base release.  Thus, this release contains packages which can
    -- be installed on a machine running the base release.
    | PrivateRelease (Release distro)
    -- ^ A private release based on another release.
    deriving (Eq, Show)

releaseName :: Distro distro => Release distro -> ReleaseName
releaseName = ReleaseName . releaseString

releaseString :: Distro distro => Release distro -> String
releaseString (PrivateRelease r) = releaseString r ++ "-private"
releaseString (ExtendedRelease r vendor) = releaseString r ++ "-" ++ distroString vendor
releaseString (Release name) = relName (_releaseName name)

-- baseReleaseString :: BaseRelease -> String
-- baseReleaseString = _releaseName

baseRelease :: Release distro -> BaseRelease
baseRelease (PrivateRelease release) = baseRelease release
baseRelease (ExtendedRelease release _) = baseRelease release
baseRelease (Release release) = release

parseReleaseName :: Distro distro => ReleaseName -> Release distro
parseReleaseName (ReleaseName s) =
    parse s
    where
      parse "sarge" = Release (BaseRelease debian (ReleaseName "sarge"))
      parse "etch" = Release (BaseRelease debian (ReleaseName "etch"))
      parse "lenny" = Release (BaseRelease debian (ReleaseName "lenny"))
      parse "squeeze" = Release (BaseRelease debian (ReleaseName "squeeze"))
      parse "wheezy" = Release (BaseRelease debian (ReleaseName "wheezy"))
      parse "jessie" = Release (BaseRelease debian (ReleaseName "jessie"))
      parse "sid" = Release (BaseRelease debian (ReleaseName "sid"))
      parse "experimental" = Release (BaseRelease debian (ReleaseName "experimental"))

      parse "dapper" = Release (BaseRelease ubuntu (ReleaseName "dapper"))
      parse "edgy" = Release (BaseRelease ubuntu (ReleaseName "edgy"))
      parse "feisty" = Release (BaseRelease ubuntu (ReleaseName "feisty"))
      parse "hardy" = Release (BaseRelease ubuntu (ReleaseName "hardy"))
      parse "intrepid" = Release (BaseRelease ubuntu (ReleaseName "intrepid"))
      parse "jaunty" = Release (BaseRelease ubuntu (ReleaseName "jaunty"))
      parse "karmic" = Release (BaseRelease ubuntu (ReleaseName "karmic"))
      parse "lucid" = Release (BaseRelease ubuntu (ReleaseName "lucid"))
      parse "maverick" = Release (BaseRelease ubuntu (ReleaseName "maverick"))
      parse "natty" = Release (BaseRelease ubuntu (ReleaseName "natty"))
      parse "oneiric" = Release (BaseRelease ubuntu (ReleaseName "oneiric"))
      parse "precise" = Release (BaseRelease ubuntu (ReleaseName "precise"))
      parse "quantal" = Release (BaseRelease ubuntu (ReleaseName "quantal"))
      parse "raring" = Release (BaseRelease ubuntu (ReleaseName "raring"))
      parse "saucy" = Release (BaseRelease ubuntu (ReleaseName "saucy"))
      parse "trusty" = Release (BaseRelease ubuntu (ReleaseName "trusty"))
      parse "utopic" = Release (BaseRelease ubuntu (ReleaseName "utopic"))
      parse "vivid" = Release (BaseRelease ubuntu (ReleaseName "vivid"))
      parse "wily" = Release (BaseRelease ubuntu (ReleaseName "wily"))
      parse "xenial" = Release (BaseRelease ubuntu (ReleaseName "xenial"))
      parse "yakkety" = Release (BaseRelease ubuntu (ReleaseName "yakkety"))
      parse "zesty" = Release (BaseRelease ubuntu (ReleaseName "zesty"))
      parse "artful" = Release (BaseRelease ubuntu (ReleaseName "artful"))

      parse _ = case spanEnd (/= '-') s of
                  (_, []) -> error $ "Unknown base release: " ++ show s
                  (base, distro) ->
                      maybe (error $ "Unknown extended release: " ++ show s)
                            (\x -> ExtendedRelease (parse (init base)) x)
                            (distroParse distro)

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p l = (\(a, b) -> (reverse b, reverse a)) $ span p (reverse l)

isPrivateRelease :: Release distro -> Bool
isPrivateRelease (PrivateRelease _) = True
isPrivateRelease _ = False
