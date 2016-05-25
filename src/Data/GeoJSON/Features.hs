{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
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
         Feature, FeatureJSON, _FeatureJSON, FeatureBSON, _FeatureBSON,
         featureGeometry, featureId, featureProperties,
         -- * FeatureCollection
         FeatureCollection, _FeatureCollectionList,
         fcEmpty, fcNew, fcInsert, fcCons, fcHead
       ) where

import           Control.Lens.Fold
import           Control.Lens.Getter
import           Control.Lens.Iso
import           Control.Lens.Prism
import           Control.Lens.Type
import           Control.Monad
import           Data.Aeson           (parseJSON, toJSON, (.:), (.:?), (.=))
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import           Data.Bson            (Field (..), cast', val)
import qualified Data.Bson            as Bson
import           Data.GeoJSON.Intern
import           Data.GeoJSON.Objects
import           Data.Maybe           (catMaybes, fromMaybe)
import           Data.Proxy
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Typeable
import           Data.Typeable.Lens


--
-- Feature
--

class FeatureType v where
  toFeatureType :: (GeoJSONObject a, BaseType t) => Feature v a t -> v

-- | A GeoJSON Feature record.
--   See '_FeatureJSON' for feature records that can be converted from/to JSON.
--   See '_FeatureBSON' for feature records that can be converted from/to BSON.
data Feature v a t where
  Feature :: (GeoJSONObject a, BaseType t) =>
             GeoJSON a t -> Maybe v -> v -> Feature v a t


featureGeometry ::
  (GeoJSONObject a, BaseType t) => Getter (Feature v a t) (GeoJSON a t)
featureGeometry = to $ \(Feature g _ _) -> g

featureId ::
  (GeoJSONObject a, BaseType t) => Getter (Feature v a t) (Maybe v)
featureId = to $ \(Feature _ i _) -> i

featureProperties ::
  (GeoJSONObject a, BaseType t) => Getter (Feature v a t) v
featureProperties = to $ \(Feature _ _ ps) -> ps


-- | feature records that can be converted from/to JSON.
type FeatureJSON = Feature Aeson.Value

-- | convert from to a JSON 'Feature'. The 3-tupel contains:
--   a 'GeoJSONObject',
--   an optional /id/,
--   a properties value
_FeatureJSON ::
  (GeoJSONObject a, BaseType t) =>
  Iso' (GeoJSON a t, Maybe Aeson.Value, Aeson.Value) (FeatureJSON a t)
_FeatureJSON = _Feature

-- | feature records that can be converted from/to BSON.
type FeatureBSON = Feature Bson.Value

-- | convert from to a BSON 'Feature'. The 3-tupel contains:
--   a 'GeoJSONObject',
--   an optional /id/,
--   a properties value
_FeatureBSON ::
  (GeoJSONObject a, BaseType t) =>
  Iso' (GeoJSON a t, Maybe Bson.Value, Bson.Value) (FeatureBSON a t)
_FeatureBSON = _Feature

_Feature ::
  (GeoJSONObject a, BaseType t) => Iso' (GeoJSON a t, Maybe v, v) (Feature v a t)
_Feature = iso (\(a, i, ps) -> Feature a i ps) (\(Feature a i ps) -> (a, i, ps))



instance BaseType t => HasFlatCoordinates (Feature v a t) t where
  flatCoordinates =  to $ \(Feature a _ _) -> a ^. flatCoordinates

instance FeatureType Aeson.Value where
  toFeatureType = toJSON

instance (GeoJSONObject a, BaseType t) => Eq (FeatureJSON a t) where
  a == b = toJSON a == toJSON b

instance (GeoJSONObject a, BaseType t) => Show (FeatureJSON a t) where
  show = show . toJSON

instance (GeoJSONObject a, BaseType t) => Aeson.ToJSON (FeatureJSON a t) where
  toJSON (Feature g mid props) = Aeson.object $ [
    typeT .= featureT,
    geometryT .= g,
    propertiesT .= props
    ] ++ maybe [] (\a -> [idT .= a]) mid

instance (GeoJSONObject a, BaseType t) => Aeson.FromJSON (FeatureJSON a t) where
  parseJSON = Aeson.withObject featureT $ \o -> do
    t <- o .: typeT
    if t /= featureT then fail $ "expected type " ++ show typeT
      else Feature <$> o .: geometryT <*> o .:? idT <*> o.: propertiesT

instance FeatureType Bson.Value where
  toFeatureType = val

instance (GeoJSONObject a, BaseType t) => Eq (FeatureBSON a t) where
  a == b = val a == val b

instance (GeoJSONObject a, BaseType t) => Show (FeatureBSON a t) where
  show = show . val

instance (GeoJSONObject a, BaseType t) => Bson.Val (FeatureBSON a t) where
  val (Feature g mid props) = Bson.Doc $ [
    typeT := val featureT,
    geometryT := val g,
    propertiesT := props
    ] ++ maybe [] (\_id -> [idBsonT := _id]) mid
  cast' (Bson.Doc d) = Feature
    <$> Bson.lookup geometryT d
    <*> pure (Bson.look idBsonT d)
    <*> Bson.look propertiesT d


--
-- Feature Collection
--

-- | a collection of multiple 'Feature'
data FeatureCollection v t where
  FCZero :: FeatureCollection v t
  FCCons ::
    (GeoJSONObject a, BaseType t) =>
    Feature v a t -> FeatureCollection v t -> FeatureCollection v t

-- | get the first 'Feature' from collection
fcHead ::
  (GeoJSONObject a, BaseType t, Typeable v) =>
  Getter (FeatureCollection v t) (Maybe (Feature v a t))
fcHead = to featureCollectionHead'
  where featureCollectionHead' ::
          (Typeable v, Typeable a) => FeatureCollection v t -> Maybe (Feature v a t)
        featureCollectionHead' FCZero = mzero
        featureCollectionHead' (FCCons f _) = cast f

-- | get the cons of the 'FeatureCollection'
fcCons :: Getter (FeatureCollection v t) (FeatureCollection v t)
fcCons = to featureCollectionCons'
  where
    featureCollectionCons' (FCCons _ xs) = xs
    featureCollectionCons' xs = xs

-- | 'Prism' from/to homogenous list
_FeatureCollectionList ::
    (GeoJSONObject a, BaseType t, Typeable v) =>
    Prism' (FeatureCollection v t) [Feature v a t]
_FeatureCollectionList =
  prism' fcFromList fcToList
  where
    fcToList FCZero = pure mempty
    fcToList fc = do
      f <- fc ^. fcHead
      fs <- fcToList (fc ^. fcCons)
      return $ mappend (pure f) fs
    fcFromList = foldl fcInsert FCZero



-- | a 'FeatureCollection' that can be converted from/to JSON.
type FeatureCollectionJSON = FeatureCollection Aeson.Value

-- | a 'FeatureCollection' that can be converted from/to BSON.
type FeatureCollectionBSON = FeatureCollection Bson.Value

-- | create an empty 'FeatureCollection'
fcEmpty :: FeatureCollection v t
fcEmpty = FCZero

-- | create a 'FeatureCollection' with an initial element.
fcNew :: (GeoJSONObject a, BaseType t) =>  Feature v a t -> FeatureCollection v t
fcNew = fcInsert FCZero

-- | insert an element into a 'FeatureCollection'
fcInsert ::
  (GeoJSONObject a, BaseType t) =>
  FeatureCollection v t -> Feature v a t -> FeatureCollection v t
fcInsert = flip FCCons


instance BaseType t => HasFlatCoordinates (FeatureCollection v t) t where
  flatCoordinates = to flatCoordinates'
    where flatCoordinates' FCZero = mempty
          flatCoordinates' (FCCons x xs) =
            mappend (x ^. flatCoordinates) (xs ^. flatCoordinates)

instance (BaseType t) => Eq (FeatureCollectionJSON t) where
  a == b = toJSON a == toJSON b

instance (BaseType t) => Show (FeatureCollectionJSON t) where
  show = show . toJSON

instance (BaseType t) => Aeson.ToJSON (FeatureCollectionJSON t) where
  toJSON fc = Aeson.object [
    typeT .= featureCollectionT,
    featuresT .= toValue fc ]
    where
      toValue FCZero = []
      toValue (FCCons x xs) = toFeatureType x : toValue xs

instance BaseType t => Aeson.FromJSON (FeatureCollectionJSON t) where
  parseJSON = Aeson.withObject featureCollectionT $ \o -> do
    t <- o .: typeT
    if t /= featureCollectionT then fail $ "expected type " ++ featureCollectionT
      else withNamedArray (T.unpack featuresT) o $ \a -> do
        fs <- sequence $ parseFC <$> a
        return $ foldr ($) FCZero fs

instance (BaseType t) => Eq (FeatureCollectionBSON t) where
  a == b = val a == val b

instance (BaseType t) => Show (FeatureCollectionBSON t) where
  show = show . val

instance BaseType t => Bson.Val (FeatureCollectionBSON t) where
  val fc = Bson.Doc [
    typeT := val featureCollectionT,
    featuresT := val (toValue fc)
    ]
    where toValue FCZero = []
          toValue (FCCons x xs) = toFeatureType x : toValue xs
  cast' (Bson.Doc d) = do
    t <- Bson.lookup typeT d
    if t /= featureCollectionT then Nothing
      else case Bson.look featuresT d of
      (Just (Bson.Array a)) -> do
        fs <- sequence $ castFC <$> a
        return $ foldr ($) FCZero fs
      _ -> Nothing
  cast' _ = Nothing

--
-- helpers
--

castFC ::
  (BaseType t, Monad m) => Bson.Value ->
  m (FeatureCollectionBSON t -> FeatureCollectionBSON t)
castFC v = case catMaybes ps of
  (x:_) -> return x
  _ -> fail "unable to cast BSON FeatureCollection"
  where ps = [
          parseFCByType (Proxy :: Proxy Point) v,
          parseFCByType (Proxy :: Proxy MultiPoint) v,
          parseFCByType (Proxy :: Proxy LineString) v,
          parseFCByType (Proxy :: Proxy LinearRing) v,
          parseFCByType (Proxy :: Proxy MultiLineString) v,
          parseFCByType (Proxy :: Proxy Polygon) v,
          parseFCByType (Proxy :: Proxy MultiPolygon) v,
          parseFCByType (Proxy :: Proxy Collection) v
          ]
        parseFCByType p = fmap FCCons . parseFeatureByType p
        parseFeatureByType :: (GeoJSONObject a, BaseType t) =>
                      Proxy a -> Bson.Value -> Maybe (FeatureBSON a t)
        parseFeatureByType _ = cast'


parseFC ::
  (BaseType t, Monad m) => Aeson.Value ->
  m (FeatureCollectionJSON t -> FeatureCollectionJSON t)
parseFC v = case catMaybes ps of
  (x:_) -> return x
  _ -> fail "unable to parse JSON FeatureCollection"
  where ps = [
          parseFCByType (Proxy :: Proxy Point) v,
          parseFCByType (Proxy :: Proxy MultiPoint) v,
          parseFCByType (Proxy :: Proxy LineString) v,
          parseFCByType (Proxy :: Proxy LinearRing) v,
          parseFCByType (Proxy :: Proxy MultiLineString) v,
          parseFCByType (Proxy :: Proxy Polygon) v,
          parseFCByType (Proxy :: Proxy MultiPolygon) v,
          parseFCByType (Proxy :: Proxy Collection) v
          ]
        parseFCByType p = fmap FCCons . parseFeatureByType p
        parseFeatureByType :: (GeoJSONObject a, BaseType t) =>
                      Proxy a -> Aeson.Value -> Maybe (FeatureJSON a t)
        parseFeatureByType _ = Aeson.parseMaybe parseJSON
