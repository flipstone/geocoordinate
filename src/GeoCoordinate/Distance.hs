{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright: Flipstone Technology Partners 2024
License: MIT
Stability: Stable

A small representation of Distance based on Meters, meant to be lightweight and specifically not
track everything possible at the type level.

@since 0.0.1.0
-}
module GeoCoordinate.Distance
  ( Distance (toMeters, fromMeters)
  , Meters (..)
  , metersFromDouble
  , metersToDouble
  , Kilometers (..)
  , kilometersFromDouble
  , kilometersToDouble
  , Miles (..)
  , milesFromDouble
  , milesToDouble
  , Degrees (..)
  , degreesFromDouble
  , degreesToDouble
  ) where

{- | Represent Degrees for use with distance calculations

@since 0.0.1.0
-}
newtype Degrees = Degrees Double
  deriving
    ( -- | @since 0.0.1.0
      Eq
    , -- | @since 0.0.1.0
      Ord
    , -- | @since 0.0.1.0
      Num
    , -- | @since 0.0.1.0
      Real
    , -- | @since 0.0.1.0
      Fractional
    , -- | @since 0.0.1.0
      RealFrac
    )

{- | Convert a 'Double' to 'Degrees' directly.

@since 0.0.1.0
-}
degreesFromDouble :: Double -> Degrees
{-# INLINEABLE degreesFromDouble #-}
degreesFromDouble = Degrees

{- | Convert a 'Degrees' to 'Double'

@since 0.0.1.0
-}
degreesToDouble :: Degrees -> Double
{-# INLINEABLE degreesToDouble #-}
degreesToDouble (Degrees d) = d

{- | Distance that can be converted to and from meters that are represented by a Double

@since 0.0.1.0
-}
class (Fractional d, Ord d) => Distance d where
  -- | @since 0.0.1.0
  toMeters :: d -> Double

  -- | @since 0.0.1.0
  fromMeters :: Double -> d

{- | Distance in meters

@since 0.0.1.0
-}
newtype Meters = Meters Double
  deriving
    ( -- | @since 0.0.1.0
      Eq
    , -- | @since 0.0.1.0
      Ord
    , -- | @since 0.0.1.0
      Num
    , -- | @since 0.0.1.0
      Real
    , -- | @since 0.0.1.0
      Fractional
    , -- | @since 0.0.1.0
      RealFrac
    )

-- | @since 0.0.1.0
instance Distance Meters where
  {-# INLINEABLE toMeters #-}
  toMeters (Meters m) = m

  {-# INLINEABLE fromMeters #-}
  fromMeters = Meters

{- | Convert a 'Double' to 'Meters' directly.

@since 0.0.1.0
-}
metersFromDouble :: Double -> Meters
{-# INLINEABLE metersFromDouble #-}
metersFromDouble = Meters

{- | Convert a 'Meters' to 'Double'

@since 0.0.1.0
-}
metersToDouble :: Meters -> Double
{-# INLINEABLE metersToDouble #-}
metersToDouble (Meters d) = d

{- | Distance in Kilometers

@since 0.0.1.0
-}
newtype Kilometers = Kilometers Double
  deriving
    ( -- | @since 0.0.1.0
      Eq
    , -- | @since 0.0.1.0
      Ord
    , -- | @since 0.0.1.0
      Num
    , -- | @since 0.0.1.0
      Real
    , -- | @since 0.0.1.0
      Fractional
    , -- | @since 0.0.1.0
      RealFrac
    )

-- | @since 0.0.1.0
instance Distance Kilometers where
  {-# INLINEABLE toMeters #-}
  toMeters (Kilometers km) = km * 1000
  {-# INLINEABLE fromMeters #-}
  fromMeters m = Kilometers (m / 1000)

{- | Convert a 'Double' to 'Kilometers' directly.

@since 0.0.1.0
-}
kilometersFromDouble :: Double -> Kilometers
{-# INLINEABLE kilometersFromDouble #-}
kilometersFromDouble = Kilometers

{- | Convert a 'Kilometers' to 'Double'

@since 0.0.1.0
-}
kilometersToDouble :: Kilometers -> Double
{-# INLINEABLE kilometersToDouble #-}
kilometersToDouble (Kilometers d) = d

{- | Distance in Miles

@since 0.0.1.0
-}
newtype Miles = Miles Double
  deriving
    ( -- | @since 0.0.1.0
      Eq
    , -- | @since 0.0.1.0
      Ord
    , -- | @since 0.0.1.0
      Num
    , -- | @since 0.0.1.0
      Real
    , -- | @since 0.0.1.0
      Fractional
    , -- | @since 0.0.1.0
      RealFrac
    )

-- | @since 0.0.1.0
instance Distance Miles where
  {-# INLINEABLE toMeters #-}
  toMeters (Miles mi) = mi * 1609.34
  {-# INLINEABLE fromMeters #-}
  fromMeters m = Miles (m / 1609.34)

{- | Convert a 'Double' to 'Miles' directly.

@since 0.0.1.0
-}
milesFromDouble :: Double -> Miles
{-# INLINEABLE milesFromDouble #-}
milesFromDouble = Miles

{- | Convert a 'Miles' to 'Double'

@since 0.0.1.0
-}
milesToDouble :: Miles -> Double
{-# INLINEABLE milesToDouble #-}
milesToDouble (Miles d) = d
