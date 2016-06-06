{-# LANGUAGE OverloadedStrings #-}

module Data.GeoJSON.Intern where

import           Data.Aeson
import qualified Data.Aeson              as Aeson
import qualified Data.Aeson.Types        as Aeson
import           Data.Bson               (Field (..))
import qualified Data.Bson               as Bson
import           Data.Scientific
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import           Data.Text.Lazy.Encoding
import qualified Data.Vector             as V
import           Database.Persist
import           GHC.Exts


jsonToBson :: (ToJSON a) => a -> Bson.Value
jsonToBson a = case toJSON a of
  (Bool b) -> Bson.Bool b
  (Number n) -> case floatingOrInteger n of
    Left f -> Bson.Float f
    Right i -> Bson.Int64 i
  (String t) -> Bson.String t
  (Array v) ->  Bson.Array . V.toList $ jsonToBson <$> v
  Null -> Bson.Null
  (Object o) -> Bson.Doc [ k := v | (k,v) <- toList . fmap jsonToBson $ o]

jsonFromBson :: FromJSON a => Bson.Value -> Maybe a
jsonFromBson a =
  case fromJSON <$> jsonFromBson' a of
    Just (Success v) -> pure v
    _ -> Nothing


jsonFromBson' :: Bson.Value -> Maybe Value
jsonFromBson' (Bson.Bool b) = pure . Bool $ b
jsonFromBson' (Bson.Float f) = pure . Number . fromFloatDigits $ f
jsonFromBson' (Bson.Int32 i) = pure . Number . fromInteger . toInteger $ i
jsonFromBson' (Bson.Int64 i) = pure . Number . fromInteger . toInteger $ i
jsonFromBson' (Bson.String t) = pure . String $ t
jsonFromBson' (Bson.Array l) =
  fmap (Array . V.fromList) . sequence $ jsonFromBson' <$> l
jsonFromBson' (Bson.Doc d) =
  let mapFields f = case jsonFromBson' . Bson.value $ f of
        Nothing -> Nothing
        Just v -> pure (Bson.label f, v)
      kvl = sequence $ mapFields <$> d
  in fmap toJSON kvl
jsonFromBson' Bson.Null = pure Null
jsonFromBson' _ = Nothing

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
