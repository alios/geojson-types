{-# LANGUAGE OverloadedStrings #-}

module Data.GeoJSON.Intern where

import           Data.Aeson       ((.:))
import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Text        (Text)
import qualified Data.Text        as T

typeT, coordinatesT, geometryT, idT, idBsonT, propertiesT, featuresT :: Text
typeT = "type"
geometryT = "geometry"
coordinatesT = "coordinates"
propertiesT = "properties"
idT = "id"
idBsonT = "_id"
featuresT = "features"

pointT, multiPointT, lineStringT, linearRingT,
  multiLineStringT, polygonT, geometriesT, multiPolygonT,
  geometryCollectionT, featureT, featureCollectionT :: String
pointT = "Point"
multiPointT = "MultiPoint"
lineStringT = "LineString"
linearRingT = "LinearRing"
multiLineStringT = "MultiLineString"
polygonT = "Polygon"
multiPolygonT = "MultiPolygon"
geometryCollectionT = "GeometryCollection"
featureT = "Feature"
featureCollectionT = "FeatureCollection"
geometriesT = "geometries"

withNamedArray ::
  String -> Aeson.Object -> (Aeson.Array -> Aeson.Parser a) -> Aeson.Parser a
withNamedArray n o p = (o .: T.pack n) >>= Aeson.withArray n p
