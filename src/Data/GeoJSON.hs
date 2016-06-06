{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GeoJSON
       ( module Data.GeoJSON.Objects,
         module Data.GeoJSON.Features,
         readFeatureCollection, readFeatureCollectionJSON
       ) where


import qualified Data.Aeson            as Aeson
import qualified Data.ByteString.Lazy  as BL
import           Data.GeoJSON.Features
import           Data.GeoJSON.Objects


-- | read a GeoJSON 'FeatureCollection' from file.
readFeatureCollection ::
  BaseType t => (HasFeature f t) => FilePath ->
  IO (Either String (FeatureCollection f t))
readFeatureCollection f = Aeson.eitherDecode <$>  BL.readFile f

readFeatureCollectionJSON ::
  (BaseGeoJSONObject a t) =>  FilePath ->
  IO (Either String (FeatureCollectionJSON a t))
readFeatureCollectionJSON = readFeatureCollection
