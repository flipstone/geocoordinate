module Generators
  ( right
  , doubleRangeGen
  , geoCoordGen
  , degreesGen
  , meterGen
  , kilometersGen
  ) where

import Data.Functor.Identity (Identity)
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified GeoCoordinate.Distance as GeoDist
import qualified GeoCoordinate.GeoCoord as Geo

right :: HH.GenT Identity (Either a b) -> HH.GenT Identity b
right = Gen.just . fmap (either (const Nothing) Just)

latGen :: HH.Gen Geo.Latitude
latGen = fmap Geo.latitudeFromDouble $ doubleRangeGen (-85) 85

lngGen :: HH.Gen Geo.Longitude
lngGen = fmap Geo.longitudeFromDouble $ doubleRangeGen (-180) 180

geoCoordGen :: HH.Gen Geo.GeoCoord
geoCoordGen =
  Geo.GeoCoord
    <$> latGen
    <*> lngGen

meterGen :: HH.Gen GeoDist.Meters
meterGen = fmap GeoDist.metersFromDouble $ doubleRangeGen 0 1000

kilometersGen :: HH.Gen GeoDist.Kilometers
kilometersGen = kilometersRangeGen 1 200

kilometersRangeGen ::
  GeoDist.Kilometers ->
  GeoDist.Kilometers ->
  HH.Gen GeoDist.Kilometers
kilometersRangeGen minValue =
  fmap GeoDist.kilometersFromDouble . doubleRangeGen (GeoDist.kilometersToDouble minValue) . GeoDist.kilometersToDouble

degreesGen :: HH.Gen GeoDist.Degrees
degreesGen = fmap GeoDist.degreesFromDouble $ doubleRangeGen 0 360

doubleRangeGen :: Double -> Double -> HH.Gen Double
doubleRangeGen start = Gen.double . Range.linearFrac start
