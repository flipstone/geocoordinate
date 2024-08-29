{- |
Copyright: Flipstone Technology Partners 2024
License: MIT
Stability: Stable

@since 0.0.1.0
-}
module GeoCoordinate
  ( Distance (toMeters, fromMeters)
  , Meters
  , metersFromDouble
  , metersToDouble
  , Kilometers
  , kilometersFromDouble
  , kilometersToDouble
  , Miles
  , milesFromDouble
  , milesToDouble
  , Degrees
  , degreesFromDouble
  , degreesToDouble
  , GeoCoord (GeoCoord)
  , latitude
  , longitude
  , Latitude
  , latitudeFromDouble
  , latitudeToDouble
  , Longitude
  , longitudeFromDouble
  , longitudeToDouble
  , hashGeoCoord
  , decodeHashedGeoCoord
  , proximitySearchHashes
  , geoDistanceWithin
  ) where

import GeoCoordinate.Distance
  ( Degrees
  , Distance (fromMeters, toMeters)
  , Kilometers
  , Meters
  , Miles
  , degreesFromDouble
  , degreesToDouble
  , kilometersFromDouble
  , kilometersToDouble
  , metersFromDouble
  , metersToDouble
  , milesFromDouble
  , milesToDouble
  )
import GeoCoordinate.GeoCoord
  ( GeoCoord (GeoCoord)
  , Latitude
  , Longitude
  , latitude
  , latitudeFromDouble
  , latitudeToDouble
  , longitude
  , longitudeFromDouble
  , longitudeToDouble
  )
import GeoCoordinate.Geohash (decodeHashedGeoCoord, hashGeoCoord)
import GeoCoordinate.ProximitySearch
  ( geoDistanceWithin
  , proximitySearchHashes
  )
