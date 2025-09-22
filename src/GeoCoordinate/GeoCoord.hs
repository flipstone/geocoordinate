{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright: Flipstone Technology Partners 2024
License: MIT
Stability: Stable

Functionality for building and consuming a type to represent geocoordinates, 'GeoCoord'.

@since 0.0.1.0
-}
module GeoCoordinate.GeoCoord
  ( GeoCoord (GeoCoord)
  , latitude
  , longitude
  , Latitude
  , latitudeFromDouble
  , latitudeToDouble
  , Longitude
  , longitudeFromDouble
  , longitudeToDouble
  ) where

import qualified Control.DeepSeq as DeepSeq

{- | A GeoCoordinate of a latitude and longitude pair.

@since 0.0.1.0
-}
data GeoCoord = GeoCoord
  { latitude :: !Latitude
  {- ^ The 'Latitude' portion of a geocoordinate.

  @since 0.0.1.0
  -}
  , longitude :: !Longitude
  {- ^ The 'Longitude' portion of a geocoordinate.

  @since 0.0.1.0
  -}
  }
  deriving (Eq, Show)

-- | @since 0.0.1.0
instance DeepSeq.NFData GeoCoord where
  {-# INLINEABLE rnf #-}
  rnf (GeoCoord lat lng) =
    DeepSeq.rnf lat `seq` DeepSeq.rnf lng

{- | Latitude as a double precision floating point number.

@since 0.0.1.0
-}
newtype Latitude = Latitude Double
  deriving
    ( -- | @since 0.0.1.0
      Eq
    , -- | @since 0.0.1.0
      Ord
    , -- | @since 0.0.1.0
      Num
    , -- | @since 0.0.1.0
      Floating
    , -- | @since 0.0.1.0
      Fractional
    , -- | @since 0.0.1.0
      Show
    , -- | @since 0.0.1.0
      DeepSeq.NFData
    )

{- | Represent a 'Double' as a 'Latitude'

@since 0.0.1.0
-}
latitudeFromDouble :: Double -> Latitude
latitudeFromDouble = Latitude

{- | Convert a 'Latitude' to a 'Double'

@since 0.0.1.0
-}
latitudeToDouble :: Latitude -> Double
latitudeToDouble (Latitude d) = d

{- | Longitude as a double precision floating point number.

@since 0.0.1.0
-}
newtype Longitude = Longitude Double
  deriving
    ( -- | @since 0.0.1.0
      Eq
    , -- | @since 0.0.1.0
      Ord
    , -- | @since 0.0.1.0
      Num
    , -- | @since 0.0.1.0
      Floating
    , -- | @since 0.0.1.0
      Fractional
    , -- | @since 0.0.1.0
      Show
    , -- | @since 0.0.1.0
      DeepSeq.NFData
    )

{- | Represent a 'Double' as a 'Longitude'

@since 0.0.1.0
-}
longitudeFromDouble :: Double -> Longitude
longitudeFromDouble = Longitude

{- | Convert a 'Longitude' to a 'Double'

@since 0.0.1.0
-}
longitudeToDouble :: Longitude -> Double
longitudeToDouble (Longitude d) = d
