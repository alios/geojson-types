{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.GeoJSON.Classes
-- Copyright   :  (C) 2016 Markus Barenhoff
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Markus Barenhoff <mbarenh@alios.org>
-- Stability   :  provisional
-- Portability :  MultiParamTypeClasses, FunctionalDependencies
--
----------------------------------------------------------------------------
module Data.GeoJSON.Classes
       ( HasGeoJSON(..)
       ) where

import Control.Lens.Getter
import Data.GeoJSON.Objects

class (GeoJSONObject a, BaseType t) => HasGeoJSON a t b | b -> t, b -> a where
  geoJSON :: Getter b (GeoJSON a t)
