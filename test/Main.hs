{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  ( main
  ) where

import qualified Data.Bytes as Bytes
import qualified Hedgehog as HH
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.Hedgehog (testProperty)

import Generators (degreesGen, doubleRangeGen, geoCoordGen, meterGen)
import GeoCoordinate.Distance (Degrees (..), Kilometers (Kilometers), Meters (Meters), toMeters)
import GeoCoordinate.Geohash (hashGeoCoord)
import GeoCoordinate.ProximitySearch (directGeo, geodeticCoord, proximitySearchHashes)

main :: IO ()
main = do
  Tasty.defaultMain test_geoSpec

test_geoSpec :: TestTree
test_geoSpec =
  testGroup
    "GeoCoord proximitySearchHashes"
    [ testProperty "contains a prefix of every lat/lng in the search area"
        . HH.property
        $ do
          coord <- HH.forAll geoCoordGen
          meters <- HH.forAll meterGen
          degrees <- HH.forAll degreesGen
          factor <- HH.forAll $ doubleRangeGen 0.0 1.0

          let
            hashes = proximitySearchHashes meters coord
            nearby = directGeo degrees (toMeters $ Meters factor * meters) (geodeticCoord coord)
            mbNearbyHash = hashGeoCoord nearby

          HH.annotateShow (hashGeoCoord coord)
          HH.annotateShow hashes
          HH.annotateShow mbNearbyHash
          case mbNearbyHash of
            Nothing -> fail "Unable to encode nearbyHash"
            Just nearbyHash -> HH.assert $ any (`Bytes.isPrefixOf` nearbyHash) hashes
    , testProperty "contains a prefix of every lat/lng in the search area (km) "
        . HH.property
        $ do
          coord <- HH.forAll geoCoordGen
          km <- HH.forAll $ doubleRangeGen 1 200
          degrees <- HH.forAll degreesGen
          factor <- HH.forAll $ doubleRangeGen 0.0 1.0

          let
            hashes = proximitySearchHashes (Kilometers km) coord
            nearby = directGeo degrees (toMeters $ Kilometers factor * Kilometers km) (geodeticCoord coord)
            mbNearbyHash = hashGeoCoord nearby

          HH.annotateShow $ hashGeoCoord coord
          HH.annotateShow hashes
          HH.annotateShow mbNearbyHash
          case mbNearbyHash of
            Nothing -> fail "Unable to encode nearbyHash"
            Just nearbyHash -> HH.assert $ any (`Bytes.isPrefixOf` nearbyHash) hashes
    , testProperty "does not search the whole world"
        . HH.property
        $ do
          coord <- HH.forAll geoCoordGen
          km <- HH.forAll $ doubleRangeGen 1 200
          let
            hashes = proximitySearchHashes (Kilometers km) coord
          HH.annotateShow hashes
          HH.assert $ all (\a -> Bytes.length a > 0) hashes
    ]

deriving instance Show Meters
deriving instance Show Degrees
