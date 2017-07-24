{-# LANGUAGE CPP #-}

-- | Specific information about known Debian-based distributions
-- and releases thereof.
module Debian.Releases
    ( BaseRelease(..)
    , baseReleaseDistro
    , baseReleaseString
    , allReleases
    , Release(..)
    , Distro(..)
    , releaseName
    , releaseString
    , baseRelease
    , distroString
    , parseReleaseName
    , isPrivateRelease
    ) where

import Control.Monad (msum)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Set (fromList, member, Set)
-- The ReleaseName type from the debian library - just a newtyped string.
import Debian.Release (ReleaseName(ReleaseName))

data BaseRelease =
    -- Debian releases
      Sarge -- 2005/6
    | Etch -- 2007/4
    | Lenny -- 2009/2
    | Squeeze -- 2011/2
    | Wheezy -- 2013/5
    | Jessie -- 2015/04
    | Stretch
    | Buster
    | Bullseye
    | Sid -- always current
    | Experimental -- always current (but actually not a base release,
                   -- not complete.  Is experimental an add-on to sid?)
    -- Ubuntu releases
    | Dapper -- 2006/10
    | Edgy -- 2007/4
    | Feisty -- 2007/10
    | Hardy -- 2008/4
    | Intrepid -- 2008/10
    | Jaunty -- 2009/4
    | Karmic -- 2009/10
    | Lucid -- 2010/4
    | Maverick -- 2010/10
    | Natty -- 2011/4
    | Oneiric -- 2011/10
    | Precise -- 2012/4
    | Quantal -- 2012/10
    | Raring -- 2013/4
    | Saucy -- 2013/10
    | Trusty -- 2014/4
    | Utopic -- 2014/10
    | Vivid -- 2015/04
    | Wily -- 2015/10
    | Xenial -- 2016/04
    | Yakkety -- 2016/10
    | Zesty -- 2017/04
    | Artful -- 2017/10
    deriving (Eq, Ord, Show, Enum, Bounded)

allReleases :: Set BaseRelease
allReleases = fromList [minBound .. maxBound]

debianReleases = fromList [minBound .. Experimental]
ubuntuReleases = fromList [Dapper .. maxBound]

-- | A Distro is any organization that provides packages.
data Distro = Ubuntu | Debian | Kanotix | SeeReason | SeeReasonGHC8 | SeeReason7 deriving (Eq, Show)

data Release
    = Release BaseRelease
    | ExtendedRelease Release Distro
    -- ^ A release which is based on another release, known as the
    -- base release.  Thus, this release contains packages which can
    -- be installed on a machine running the base release.
    | PrivateRelease Release
    -- ^ A private release based on another release.
    deriving (Eq, Show)

releaseDistro :: Release -> Distro
releaseDistro (ExtendedRelease _ distro) = distro
releaseDistro (PrivateRelease release) = releaseDistro release
releaseDistro (Release release) = baseReleaseDistro release

baseReleaseDistro :: BaseRelease -> Distro
baseReleaseDistro x =
    if x `member` debianReleases
    then Debian
    else if x `member` ubuntuReleases
         then Ubuntu
         else error $ "baseReleaseDistro " ++ show x

releaseName :: Release -> ReleaseName
releaseName = ReleaseName . releaseString

releaseString :: Release -> String
releaseString (PrivateRelease r) = releaseString r ++ "-private"
releaseString (ExtendedRelease r vendor) = releaseString r ++ "-" ++ distroString vendor
releaseString (Release name) = baseReleaseString name

baseReleaseString :: BaseRelease -> String
baseReleaseString name = map toLower (show name)

distroString :: Distro -> String
distroString SeeReason = "seereason"
distroString Ubuntu = "ubuntu"
distroString Debian = "debian"
distroString Kanotix = "kanotix"
distroString SeeReasonGHC8 = "seereason"
distroString SeeReason7 = "seereason7"

baseRelease :: Release -> BaseRelease
baseRelease (PrivateRelease release) = baseRelease release
baseRelease (ExtendedRelease release _) = baseRelease release
baseRelease (Release release) = release

parseReleaseName :: ReleaseName -> Release
parseReleaseName (ReleaseName s) =
    parse s
    where
      parse "sarge" = Release Sarge
      parse "etch" = Release Etch
      parse "lenny" = Release Lenny
      parse "squeeze" = Release Squeeze
      parse "wheezy" = Release Wheezy
      parse "jessie" = Release Jessie
      parse "sid" = Release Sid
      parse "experimental" = Release Experimental

      parse "dapper" = Release Dapper
      parse "edgy" = Release Edgy
      parse "feisty" = Release Feisty
      parse "hardy" = Release Hardy
      parse "intrepid" = Release Intrepid
      parse "jaunty" = Release Jaunty
      parse "karmic" = Release Karmic
      parse "lucid" = Release Lucid
      parse "maverick" = Release Maverick
      parse "natty" = Release Natty
      parse "oneiric" = Release Oneiric
      parse "precise" = Release Precise
      parse "quantal" = Release Quantal
      parse "raring" = Release Raring
      parse "saucy" = Release Saucy
      parse "trusty" = Release Trusty
      parse "utopic" = Release Utopic
      parse "vivid" = Release Vivid
      parse "wily" = Release Wily
      parse "xenial" = Release Xenial
      parse "yakkety" = Release Yakkety
      parse "zesty" = Release Zesty
      parse "artful" = Release Artful

      parse s =
#if 1
       fromMaybe (error $ "Unexpected release name: " ++ show s)
          (msum (fmap (\(f, suf) -> fmap f (viewSuffix suf s))
                     [(PrivateRelease . parse, "-private"),
                      (\prefix -> ExtendedRelease (parse prefix) SeeReason, "-seereason"),
                      (\prefix -> ExtendedRelease (parse prefix) SeeReasonGHC8, "-ghc8"),
                      (\prefix -> ExtendedRelease (parse prefix) SeeReason7, "-seereason7")]))

#else
          case viewSuffix "-private" s of
            Just prefix -> PrivateRelease (parse prefix)
            Nothing ->
                case viewSuffix "-seereason" s of
                  Just prefix -> ExtendedRelease (parse prefix) SeeReason
                  Nothing ->
                      case viewSuffix "-ghc8" s of
                        Just prefix -> ExtendedRelease (parse prefix) SeeReasonGHC8
                        Nothing -> error $ "Unexpected release name: " ++ show s
#endif

isPrivateRelease (PrivateRelease _) = True
isPrivateRelease _ = False

viewSuffix :: String -> String -> Maybe String
viewSuffix suffix string =
    case splitAt (length string - length suffix) string of
      (pre, suf) | suf == suffix -> Just pre
      _ -> Nothing
