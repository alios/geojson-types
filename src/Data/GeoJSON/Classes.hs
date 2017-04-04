{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.GeoJSON.Classes
-- Copyright   :  (C) 2016 Markus Barenhoff
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Markus Barenhoff <mbarenh@alios.org>
-- Stability   :  provisional
-- Portability :  ConstraintKinds, FunctionalDependencies, KindSignatures, TypeFamilies
--
----------------------------------------------------------------------------

module Data.GeoJSON.Classes where

import           Control.Applicative
import           Control.Lens.Getter
import           Control.Lens.Iso
import           Control.Lens.Lens
import           Control.Lens.Review
import           Data.Aeson
import           Data.Aeson.Types
import           Data.GeoJSON.Intern
import           Data.Proxy
import           Data.Text           (Text)

type BaseType a = (Num a, Ord a, Eq a, ToJSON a, FromJSON a)

class (ToJSON i, FromJSON i, BaseType a) => IsGeometry g i a | g a -> i where
  type GeometryStructure g a (fv :: * -> *) (fc :: * -> *) :: *
  data Geometry g a :: *
  _Geometry     :: Iso' i (Geometry g a)
  geometryType  :: Proxy (g, a) -> Text

type PositionStructureClass fc a =
  (BaseType a, Traversable fc, Applicative fc, Monoid (fc a))

type VectorStructureClass fv fc a =
  ( PositionStructureClass fc a, Traversable fv
  , Applicative fv, Monoid (fv (fc a)), Monoid (fv (fv (fc a)))
  )




type FeatureId = Either Text Integer

class HasFeatureId f where
  featureId :: Lens' f (Maybe FeatureId)

class HasProperties f where
  properties :: Lens' f (Maybe Object)



instance (IsGeometry g i a) => ToJSON (Geometry g a) where
  toJSON = toGeometryJSON

toGeometryJSON :: IsGeometry g i a => Geometry g a -> Value
toGeometryJSON = toGeometryJSON' Proxy
  where toGeometryJSON' :: IsGeometry g i a => Proxy (g, a) -> Geometry g a -> Value
        toGeometryJSON' pga g = object
          [ typeField .=   geometryType pga
          , coordinatesField .= review _Geometry g
          ]

instance IsGeometry g i a => FromJSON (Geometry g a) where
  parseJSON = parseGeometryJSON

parseGeometryJSON :: IsGeometry g i a => Value -> Parser (Geometry g a)
parseGeometryJSON = parseGeometryJSON' Proxy
  where
    parseGeometryJSON' :: IsGeometry g i a =>
      Proxy (g, a) -> Value -> Parser (Geometry g a)
    parseGeometryJSON' pga (Object a) = do
      t <- a .: typeField
      if t /= geometryType pga then empty
        else view _Geometry <$> a .: coordinatesField
    parseGeometryJSON' _ _  = empty
