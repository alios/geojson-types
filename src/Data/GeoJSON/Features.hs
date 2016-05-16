{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
       ( Feature, FeatureJSON, _FeatureJSON, FeatureBSON, _FeatureBSON
       , FeatureCollection, fcZero, fcNew, fcInsert
       ) where

import Data.Maybe (fromMaybe, catMaybes)
import Control.Lens.Prism
import Control.Lens.Fold
import Control.Lens.Getter
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson (toJSON, parseJSON, (.=), (.:), (.:?))
import Data.Bson (Field(..), cast', val)
import qualified Data.Bson as Bson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy
import Control.Lens.Iso

import Data.GeoJSON.Objects
import Data.GeoJSON.Intern


--
-- Feature
--
class FeatureType v where
  toFeatureType :: (GeoJSONObject a, BaseType t) => Feature v a t -> v
  
data Feature v a t where
  Feature :: (GeoJSONObject a, BaseType t) =>
             GeoJSON a t -> Maybe v -> v -> Feature v a t


_FeatureJSON ::
  (GeoJSONObject a, BaseType t) =>
  Iso' (GeoJSON a t, Maybe Aeson.Value, Aeson.Value) (FeatureJSON a t)
_FeatureJSON = _Feature

_FeatureBSON ::
  (GeoJSONObject a, BaseType t) =>
  Iso' (GeoJSON a t, Maybe Bson.Value, Bson.Value) (FeatureBSON a t)
_FeatureBSON = _Feature

_Feature ::
  (GeoJSONObject a, BaseType t) => Iso' (GeoJSON a t, Maybe v, v) (Feature v a t)
_Feature = iso (\(a, i, ps) -> Feature a i ps) (\(Feature a i ps) -> (a, i, ps))


instance BaseType t => HasFlatCoordinates (Feature v a t) t where
  flatCoordinates =  to $ \(Feature a _ _) -> a ^. flatCoordinates
  
type FeatureJSON = Feature Aeson.Value

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
    ] ++ _id
    where
      _id = case mid of
        Nothing -> []
        Just a -> [ idT .= a ]
        
instance (GeoJSONObject a, BaseType t) => Aeson.FromJSON (FeatureJSON a t) where
  parseJSON = Aeson.withObject featureT $ \o -> do
    t <- o .: typeT
    if t /= featureT then fail $ "expected type " ++ show typeT
      else Feature <$> o .: geometryT <*> o .:? idT <*> o.: propertiesT
           
type FeatureBSON = Feature Bson.Value

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

data FeatureCollection v t where
  FCZero :: FeatureCollection v t
  FCCons ::
    (GeoJSONObject a, BaseType t) =>
    Feature v a t -> FeatureCollection v t -> FeatureCollection v t

type FeatureCollectionJSON = FeatureCollection Aeson.Value

fcZero :: FeatureCollection v t
fcZero = FCZero

fcInsert :: (GeoJSONObject a, BaseType t) => FeatureCollection v t -> Feature v a t -> FeatureCollection v t
fcInsert = flip FCCons

fcNew :: (GeoJSONObject a, BaseType t) =>  Feature v a t -> FeatureCollection v t
fcNew = fcInsert FCZero

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
    featuresT .= toValue fc
    ]
    where toValue FCZero = []
          toValue (FCCons x xs) = toFeatureType x : toValue xs

instance BaseType t => Aeson.FromJSON (FeatureCollectionJSON t) where
  parseJSON = Aeson.withObject featureCollectionT $ \o -> do
    t <- o .: typeT
    if t /= featureCollectionT then fail $ "expected type " ++ featureCollectionT
      else withNamedArray (T.unpack featuresT) o $ \a -> do
        fs <- sequence $ parseFC <$> a
        return $ foldr ($) FCZero fs

type FeatureCollectionBSON = FeatureCollection Bson.Value

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



       
