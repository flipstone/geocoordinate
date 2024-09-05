{- |
Copyright: Flipstone Technology Partners 2024
License: MIT
Stability: Stable

Proximity searching of 'GeoCoord' based on geohashing.

@since 0.0.1.0
-}
module GeoCoordinate.ProximitySearch
  ( proximitySearchHashes
  , geoDistanceWithin
  , geodeticCoord
  , directGeo
  ) where

import qualified Data.Bytes as Bytes
import qualified Data.Maybe as Maybe
import qualified Data.Set as S
import qualified Geodetics.Geodetic as Geodetic
import qualified Geodetics.Path as GeoPath
import qualified Numeric.Units.Dimensional.Prelude as DimPrelude

import GeoCoordinate.Distance (Degrees, Distance, degreesFromDouble, degreesToDouble, fromMeters, toMeters)
import GeoCoordinate.GeoCoord (GeoCoord (GeoCoord, latitude, longitude), latitudeFromDouble, latitudeToDouble, longitudeFromDouble, longitudeToDouble)
import GeoCoordinate.Geohash (hashGeoCoord)

{- ** WARNING**
 -
 - proximitySearchHashes is tuned to work for search distance
 - up to 200km, between latitude -85 and 85. The Arbitrary
 - instances in the specs reflect this.
 -
 - Counterexamples can sometimes take tens or hundreds of
 - thousands of test cases to find, so if you need to touch
 - this function, be sure to run lots of test cases.
 -
 - The spec file is configured to run 100 times as many cases on
 - this function as normal. Running with normal set to 1000 (thus
 - running 100,000 proximitySearchHashes) test cases should be
 - sufficient, or ten times that if you're extra paranoid.
 -
 -    cabal test --test-option=--qc-max-success=1000
-}

{- | Builds a bounding box with the given distance around the given GeoCoord. Note that the geohashing
   algorithm will use 7 base-32 digits for precision, giving roughly 76m of error. See
   https://en.wikipedia.org/wiki/Geohash for precision information.

@since 0.0.1.0
-}
proximitySearchHashes :: Distance d => d -> GeoCoord -> S.Set Bytes.Bytes
{-# INLINEABLE proximitySearchHashes #-}
proximitySearchHashes dist =
  proximitySearchHashesDouble (toMeters dist)

{- This is a type restricted proximity search. This is done to discharge the typeclass as quickly as
 possible and allow for the smaller body to be inlined. Do NOT export this version as export has
 pessimizing impacts on the GHC optimizer. Also do not mark this for inlining as it is relatively
 large function.

-}
proximitySearchHashesDouble :: Double -> GeoCoord -> S.Set Bytes.Bytes
proximitySearchHashesDouble distMeters coord =
  let
    conjoin areas
      | c == 1 = areas
      | c == 2 = areas
      | c == 4 = areas
      | otherwise = conjoin (S.foldl' keepInits mempty areas)
     where
      c = S.size areas

    keepInits accum bytes =
      case Bytes.unsnoc bytes of
        Nothing -> accum
        Just (keep, _) -> S.insert keep accum

    candidates = Maybe.mapMaybe hashGeoCoord [coord, dn, ne, de, se, ds, sw, dw, nw]

    geodetic = geodeticCoord coord
    dn = directGeo (degreesFromDouble 0) distMeters geodetic
    de = directGeo (degreesFromDouble 90) distMeters geodetic
    ds = directGeo (degreesFromDouble 180) distMeters geodetic
    dw = directGeo (degreesFromDouble 270) distMeters geodetic

    ne = GeoCoord n e
    se = GeoCoord s e
    sw = GeoCoord s w
    nw = GeoCoord n w

    n = latitude dn
    e = longitude de
    s = latitude ds
    w = longitude dw
  in
    conjoin $ S.fromList candidates

{- | Given an origin point and a distance, is a given destination within that distance

@since 0.0.1.0
-}
geoDistanceWithin :: Distance d => GeoCoord -> d -> GeoCoord -> Bool
{-# INLINEABLE geoDistanceWithin #-}
geoDistanceWithin origin dist =
  (>=) dist . geoDistance origin

{- | Convert a 'GeoCoord' to a WGS84 representation.

@since 0.0.1.0
-}
geodeticCoord :: GeoCoord -> Geodetic.Geodetic Geodetic.WGS84
geodeticCoord geocoord =
  Geodetic.Geodetic
    (fmap latitudeToDouble $ multiplyByUnit (latitude geocoord) DimPrelude.degree)
    (fmap longitudeToDouble $ multiplyByUnit (longitude geocoord) DimPrelude.degree)
    (multiplyByUnit 0 DimPrelude.meter)
    Geodetic.WGS84

geoDistance :: Distance d => GeoCoord -> GeoCoord -> d
{-# INLINEABLE geoDistance #-}
geoDistance g1 =
  fromMeters . geoDistanceDouble g1

{- This is a type restricted distance, that always returns a 'Double'. This is done to discharge the
 typeclass as quickly as possible and allow for the smaller body to be inlined. Do NOT export this
 version as export has pessimizing impacts on the GHC optimizer. Also do not mark this for inlining
 as it is relatively large function.

-}
geoDistanceDouble :: GeoCoord -> GeoCoord -> Double
geoDistanceDouble g1 g2 =
  if g1 == g2
    then 0.0
    else
      let
        result =
          case Geodetic.groundDistance (geodeticCoord g1) (geodeticCoord g2) of
            Just (dist, _, _) -> divideByUnit dist DimPrelude.meter
            Nothing -> infinity
      in
        result

-- TODO SC-38245 we should use TH/QQ here rather than leave this to runtime. There is just no reason
-- to have to compute this.
infinity :: Double
infinity = DimPrelude.read "Infinity"

{- | Obtain the 'GeoCoord' by looking at an angle and distance along a curve.

@since 0.0.1.0
-}
directGeo :: Degrees -> Double -> Geodetic.Geodetic Geodetic.WGS84 -> GeoCoord
directGeo degrees d geodetic =
  let
    ang = degreesToDouble degrees
    path = GeoPath.rayPath geodetic (multiplyByUnit ang DimPrelude.degree) (multiplyByUnit 0 DimPrelude.degree)
    (dest, _, _) = GeoPath.pathFunc path (multiplyByUnit d DimPrelude.meter)
    lat = latitudeFromDouble $ divideByUnit (Geodetic.latitude dest) DimPrelude.degree
    lng = longitudeFromDouble $ divideByUnit (Geodetic.longitude dest) DimPrelude.degree
  in
    GeoCoord lat lng

multiplyByUnit :: Fractional a => a -> DimPrelude.Unit m d a -> DimPrelude.Quantity d a
multiplyByUnit = (DimPrelude.*~)

divideByUnit :: Fractional a => DimPrelude.Quantity d a -> DimPrelude.Unit m d a -> a
divideByUnit x y = x DimPrelude./~ y
