{-# LANGUAGE OverloadedStrings #-}

module Data.GeoJSON.Intern where

import           Data.Aeson
import qualified Data.Aeson              as Aeson
import qualified Data.Aeson.Types        as Aeson
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import           Data.Text.Lazy.Encoding
import           Database.Persist
import           GHC.Exts

jsonToPersistValue :: (ToJSON a) => a -> PersistValue
jsonToPersistValue p = case jsonToPersistValue' . toJSON $ p of
  Error err -> error $ "jsonToPersistValue: " ++ show err
  Success a -> a

jsonToPersistValue' :: Aeson.Value -> Result PersistValue
jsonToPersistValue' (Aeson.String t) = Success (PersistText t)
jsonToPersistValue' (Aeson.Array a) =
  case sequence . foldr (mappend . pure . jsonToPersistValue') mempty $ a of
    Success a' -> return . PersistList $ a'
    Error err -> Error err
jsonToPersistValue' (Aeson.Object o) =
  case sequence $ jsonToPersistValue' <$> o of
    Success o' -> return . PersistMap . toList $ o'
    Error err -> Error err
jsonToPersistValue' a = fromJSON a


persistFieldToValue' :: (PersistField a) => a -> Aeson.Value
persistFieldToValue' a = case toPersistValue a of
  (PersistText t) -> Aeson.String t
  (PersistMap m) -> object [ k .= persistFieldToValue' v | (k,v) <- m]
  (PersistList l) ->
    let toV = foldr (mappend . pure . persistFieldToValue') mempty
    in Aeson.Array . toV $ l
  a' -> toJSON a'

persistFieldToValue :: (PersistField a, FromJSON b) => a  -> Either T.Text b
persistFieldToValue a = case fromJSON . persistFieldToValue' $ a of
  Error e -> Left . T.pack $ e
  Success b -> Right b



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

showJSON :: ToJSON a => a -> String
showJSON = TL.unpack . decodeUtf8 . encode
