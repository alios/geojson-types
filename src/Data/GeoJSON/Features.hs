{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.GeoJSON.Features
-- Copyright   :  (C) 2016 Markus Barenhoff
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Markus Barenhoff <mbarenh@alios.org>
-- Stability   :  provisional
-- Portability :  FunctionalDependencies, TemplateHaskell
----------------------------------------------------------------------------
module Data.GeoJSON.Features
  ( HasFeature(..), Feature, nullFeature
  , HasFeatureObject(..), FeatureObject, nullFeatureObject
  , FeatureCollection, _FeatureCollection
  ) where

import           Control.Applicative
import           Control.Lens.Getter
import           Control.Lens.Prism
import           Data.Vector (Vector)
import           Control.Lens.TH
import           Data.Aeson              as Aeson
import           Data.GeoJSON.Classes
import           Data.GeoJSON.Geometries
import           Data.GeoJSON.Intern
import           Data.GeoJSON.Position

data Feature g a = Feature {
  __featureId        :: Maybe FeatureId,
  _featureGeometry   :: Maybe (Geometry g a),
  _featureProperties :: Maybe Object
  }

makeClassy ''Feature

nullFeature :: (IsGeometry g i a) => Feature g a
nullFeature = Feature Nothing Nothing Nothing

data FeatureObject a = FeatureObject {
  _featureObjectId         :: Maybe FeatureId,
  _featureObjectGeometry   :: Maybe (GeometryObject a),
  _featureObjectProperties :: Maybe Object
  }

makeClassy ''FeatureObject

nullFeatureObject :: (BaseType a) => FeatureObject a
nullFeatureObject = FeatureObject Nothing Nothing Nothing



instance HasFeatureId (Feature g a) where
  featureId = _featureId

instance HasFeatureId (FeatureObject a) where
  featureId = featureObjectId

instance HasProperties (Feature g a) where
  properties = featureProperties

instance HasProperties (FeatureObject a) where
  properties = featureObjectProperties



instance (HasBoundingBox(Geometry g a) a) =>
  HasBoundingBox (Feature g a) a where
  boundingBox f = maybe mempty boundingBox $ view featureGeometry f

instance (HasBoundingBox(GeometryObject a) a) =>
  HasBoundingBox (FeatureObject a) a where
  boundingBox f = maybe mempty boundingBox $ view featureObjectGeometry f


instance (IsGeometry g i a) => ToJSON (Feature g a) where
  toJSON = hasFeatureToJSON

instance (BaseType a) => ToJSON (FeatureObject a) where
  toJSON = hasFeatureObjectToJSON

instance (IsGeometry g i a) => FromJSON (Feature g a) where
  parseJSON (Object a) = do
    t <- a .: typeField
    if t /= _feature then empty
      else Feature <$>
           a .:? idField <*> a .:? geometryField <*> a .:? propertiesField
  parseJSON _          = empty

instance (BaseType a) => FromJSON (FeatureObject a) where
  parseJSON (Object a) = do
    t <- a .: typeField
    if t /= _feature then empty
      else FeatureObject <$>
           a .:? idField <*> a .:? geometryField <*> a .:? propertiesField
  parseJSON _          = empty


hasFeatureToJSON :: (HasFeature s g a, IsGeometry g i a) => s -> Value
hasFeatureToJSON f =
  let a = f ^. feature
  in object [
  typeField .= _feature,
  geometryField .= (a ^. featureGeometry),
  propertiesField .= (a ^. featureProperties),
  idField .= (a ^. featureId)
  ]

hasFeatureObjectToJSON :: (HasFeatureObject s a, BaseType a) => s -> Value
hasFeatureObjectToJSON f =
  let a = f ^. featureObject
  in object [
  typeField .= _feature,
  geometryField .= (a ^. featureObjectGeometry),
  propertiesField .= (a ^. featureObjectProperties),
  idField .= (a ^. featureObjectId)
  ]


newtype FeatureCollection g a =
  FeatureCollection (Vector (Feature g a))

newtype FeatureCollectionObject a =
  FeatureCollectionObject (Vector (FeatureObject a))


_FeatureCollectionObject ::
  (BaseType a, Applicative f, Foldable f, Monoid (f (FeatureObject a))) =>
  Prism' (FeatureCollectionObject a) (f (FeatureObject a))
_FeatureCollectionObject = prism' f t
  where f = FeatureCollectionObject . foldMap pure
        t (FeatureCollectionObject a) = pure $ foldMap pure a

_FeatureCollection ::
  (BaseType a, Applicative f, Foldable f, Monoid (f (Feature g a))) =>
  Prism' (FeatureCollection g a) (f (Feature g a))
_FeatureCollection = prism' f t
  where f = FeatureCollection . foldMap pure
        t (FeatureCollection a) = pure $ foldMap pure a


instance (BaseType a) => HasBoundingBox (FeatureCollectionObject a) a where
  boundingBox (FeatureCollectionObject a) = foldMap boundingBox a

instance (HasBoundingBox (Geometry g a) a) =>
  HasBoundingBox (FeatureCollection g a) a where
  boundingBox (FeatureCollection a) = foldMap boundingBox a

instance BaseType a => ToJSON (FeatureCollectionObject a) where
  toJSON (FeatureCollectionObject a) = object [
    typeField .= _featureCollection,
    featuresField  .= a
    ]

instance IsGeometry g i a => ToJSON (FeatureCollection g a) where
  toJSON (FeatureCollection a) = object [
    typeField .= _featureCollection,
    featuresField  .= a
    ]

instance (BaseType a) => FromJSON (FeatureCollectionObject a) where
  parseJSON (Object a) = do
    t <- a .: typeField
    if t /= _featureCollection then empty
      else FeatureCollectionObject <$> a .: featuresField
  parseJSON _          = empty

instance (IsGeometry g i a) => FromJSON (FeatureCollection g a) where
  parseJSON (Object a) = do
    t <- a .: typeField
    if t /= _featureCollection then empty
      else FeatureCollection <$> a .: featuresField
  parseJSON _          = empty
