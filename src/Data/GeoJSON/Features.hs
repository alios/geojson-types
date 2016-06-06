{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.GeoJSON.Features
-- Copyright   :  (C) 2016 Markus Barenhoff
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Markus Barenhoff <mbarenh@alios.org>
-- Stability   :  provisional
-- Portability :  FunctionalDependencies,
--                TypeFamilies,
--                GADTs
--                RankNTypes
--
----------------------------------------------------------------------------
module Data.GeoJSON.Features
       ( -- * Feature
         HasFeature(..), Feature, _Feature, mapFeature,
         -- * JSON Feature
         HasFeatureJSON(..), FeatureJSON, _FeatureJSON, mapFeatureJSON,
         -- * Feature Collection
         FeatureCollection, FeatureCollectionJSON,
         _FeatureCollection, mapFeatureCollection
       ) where

import           Control.Lens.Getter
import           Control.Lens.Lens
import           Control.Lens.Setter  hiding ((.=))
import           Control.Lens.TH
import           Data.Aeson           ((.:), (.=))
import qualified Data.Aeson           as Aeson
import qualified Data.Bson            as Bson
import           Data.GeoJSON.Intern
import           Data.GeoJSON.Objects
import           Data.Typeable
import           Data.Vector          (Vector)
import           Database.Persist
import           Database.Persist.Sql
import           GHC.Exts
import           GHC.Generics         (Generic)

--
-- Feature
--

newtype Feature f t
  = Feature (FeatureProperties f, GeoJSON (FeatureType f) t)



class ( BaseType t
      , BaseGeoJSONObject (FeatureType f) t
      , Aeson.ToJSON (FeatureProperties f)
      , Aeson.FromJSON (FeatureProperties f)
      , Typeable f
      ) => HasFeature f t | f -> t where
  data FeatureProperties f :: *
  type FeatureType f :: *

  featureProperties :: Lens' f (FeatureProperties f)
  featureGeometry :: Lens' f (GeoJSON (FeatureType f) t)

instance (HasFeature f t) => HasFlatCoordinates (Feature f t) t where
  flatCoordinates = to fc
    where fc (Feature (_, g)) = g ^. flatCoordinates

mapFeature :: (HasFeature f t, BaseType s) =>
              (t -> s) -> Feature f t -> Feature f s
mapFeature f (Feature (ps, g)) = Feature (ps, mapGeoJSON f g)


toFeature :: HasFeature s t => s -> Feature s t
toFeature f = Feature (f ^. featureProperties, f ^. featureGeometry)

instance HasFeature f t => Aeson.ToJSON (Feature f t) where
  toJSON (Feature (ps, g)) = Aeson.object [
    typeT .= featureT,
    propertiesT .= ps,
    geometryT .= g
    ]

instance (HasFeature f t) => Aeson.FromJSON (Feature f t) where
  parseJSON = Aeson.withObject featureT $ \o -> do
    t <- o .: typeT
    if featureT /= t
      then fail . mconcat $ ["expected ", featureT, " got ", t ]
      else Feature <$> ((,) <$> o .: propertiesT <*> o .: geometryT)

instance (HasFeature f t) => Show (Feature f t) where
  show = showJSON

instance (HasFeature f t) => Eq (Feature f t) where
  a == b = Aeson.toJSON a == Aeson.toJSON b

instance HasFeature f t => PersistField (Feature f t) where
  toPersistValue = jsonToPersistValue
  fromPersistValue = persistFieldToValue

instance HasFeature f t => PersistFieldSql (Feature f t) where
  sqlType _ = SqlString

instance HasFeature f t => Bson.Val (Feature  f t) where
  val = jsonToBson
  cast' = jsonFromBson

updateFeatureJSON :: (HasFeature f t, Monad m) => Aeson.Value -> f -> m f
updateFeatureJSON b a = case updateFeatureJSON' b a of
  Aeson.Success a' -> return a'
  Aeson.Error e -> fail e

updateFeatureJSON' :: (HasFeature f t) => Aeson.Value -> f -> Aeson.Result f
updateFeatureJSON' a b = do
  (Feature (ps, g)) <- Aeson.fromJSON a
  maybe (fail "unable to cast to Feature") return . cast . setFeature ps g $ b
  where
    setFeature ps g = set featureProperties ps . set featureGeometry g

data FeatureJSON a t =
  FeatureJSON {
    _featureJSONProperties :: Aeson.Value,
    _featureJSONGeometry   :: GeoJSON a t
    }
makeClassy ''FeatureJSON
makePrisms ''FeatureJSON


mapFeatureJSON :: (BaseGeoJSONObject a t, BaseType s) =>
              (t -> s) -> FeatureJSON a t -> FeatureJSON a s
mapFeatureJSON f (FeatureJSON ps g) = FeatureJSON ps (mapGeoJSON f g)


instance BaseGeoJSONObject a t => HasFeature (FeatureJSON a t) t where
  data FeatureProperties (FeatureJSON a t) =
    MkJSONFeature Aeson.Value
    deriving (Typeable, Generic)
  type FeatureType (FeatureJSON a t) = a
  featureProperties = lens g s
    where g = MkJSONFeature . view featureJSONProperties
          s f (MkJSONFeature v) = set featureJSONProperties v f
  featureGeometry = featureJSONGeometry

instance Aeson.ToJSON (FeatureProperties (FeatureJSON a t))
instance Aeson.FromJSON (FeatureProperties (FeatureJSON a t))

instance BaseGeoJSONObject a t => Aeson.ToJSON (FeatureJSON a t) where
  toJSON = Aeson.toJSON . toFeature

instance BaseGeoJSONObject a t => Aeson.FromJSON (FeatureJSON a t) where
  parseJSON = flip updateFeatureJSON (FeatureJSON undefined undefined)

instance BaseGeoJSONObject a t => Show (FeatureJSON a t) where
  show = showJSON

instance BaseGeoJSONObject a t => Eq (FeatureJSON a t) where
  a == b = Aeson.toJSON a == Aeson.toJSON b

instance BaseGeoJSONObject a t => PersistField (FeatureJSON a t) where
  toPersistValue = jsonToPersistValue
  fromPersistValue = persistFieldToValue

instance BaseGeoJSONObject a t => PersistFieldSql (FeatureJSON a t) where
  sqlType _ = SqlString

--
-- FeatureCollection
--

newtype FeatureCollection f t =
  FeatureCollection (Vector (Feature f t))

makePrisms ''FeatureCollection

type FeatureCollectionJSON a t = FeatureCollection (FeatureJSON a t) t

mapFeatureCollection ::
  (HasFeature f t, BaseType s) =>
  (t -> s) -> FeatureCollection f t -> FeatureCollection f s
mapFeatureCollection f (FeatureCollection fc) =
  FeatureCollection (mapFeature f <$> fc)

instance (HasFeature f t) => Aeson.ToJSON (FeatureCollection f t) where
  toJSON (FeatureCollection fc) =
    Aeson.object [ typeT .= featureCollectionT, featuresT .= fc ]

instance (HasFeature f t) => Aeson.FromJSON (FeatureCollection f t) where
  parseJSON = Aeson.withObject featureCollectionT $ \o -> do
    t <- o .: typeT
    if featureCollectionT /= t
      then fail . mconcat $ ["expected ", featureCollectionT, " got ", t ]
      else FeatureCollection <$> o .: featuresT

instance (HasFeature f t) => Show (FeatureCollection f t) where
  show = showJSON

instance (HasFeature f t) => Eq (FeatureCollection f t) where
  a == b = Aeson.toJSON a == Aeson.toJSON b

instance (HasFeature f t) => HasFlatCoordinates (FeatureCollection f t) t where
  flatCoordinates = to gfc
    where gfc (FeatureCollection fc) =
            mconcat . fmap (view flatCoordinates) . toList $ fc

instance HasFeature f t => Bson.Val (FeatureCollection  f t) where
  val = jsonToBson
  cast' = jsonFromBson


makePrisms ''Feature
