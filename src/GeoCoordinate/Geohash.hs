{- |
Copyright: Flipstone Technology Partners 2024
License: MIT
Stability: Stable

Functionality for performing a geohash on and decoding a geohash to a 'GeoCoord'

@since 0.0.1.0
-}
module GeoCoordinate.Geohash
  ( hashGeoCoord
  , decodeHashedGeoCoord
  ) where

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Text.Latin1 as BytesL
import qualified Data.Geohash as Geohash

import qualified GeoCoordinate.GeoCoord as GeoCoord

{- | Create a geohash with a set precision of 7.

@since 0.0.1.0
-}
hashGeoCoord :: GeoCoord.GeoCoord -> Maybe Bytes.Bytes
hashGeoCoord geoCoord =
  fmap BytesL.fromString $ Geohash.encode 7 (GeoCoord.latitudeToDouble (GeoCoord.latitude geoCoord), GeoCoord.longitudeToDouble (GeoCoord.longitude geoCoord))

{- | Attempt to decode a previously hashed value back to a 'GeoCoord.GeoCoord'.

@since 0.0.1.0
-}
decodeHashedGeoCoord :: Bytes.Bytes -> Maybe GeoCoord.GeoCoord
decodeHashedGeoCoord =
  let
    mkGeo (latDouble, longDouble) = GeoCoord.GeoCoord (GeoCoord.latitudeFromDouble latDouble) (GeoCoord.longitudeFromDouble longDouble)
  in
    fmap mkGeo . Geohash.decode . BytesL.toString
