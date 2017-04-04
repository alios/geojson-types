{-# LANGUAGE OverloadedStrings #-}
module Data.GeoJSON.Intern  where


import           Control.Applicative
import           Control.Lens.Iso
import           Control.Lens.Operators hiding ((.=))
import           Data.Aeson             as Aeson
import           Data.Aeson.Types       as Aeson
import           Data.Bson              as Bson
import qualified Data.HashMap.Strict    as Map
import           Data.Scientific
import           Data.Text              (Text)

typeField, coordinatesField, geometriesField, geometryField
  , propertiesField, idField, featuresField :: Text
coordinatesField = "coordinates"
typeField = "type"
geometriesField = "geometries"
geometryField = "geometry"
idField = "id"
propertiesField = "properties"
featuresField = "features"

_point, _multiPoint, _lineString, _polygon
  , _multiLineString, _multiPolygon, _geometryCollection
  , _feature, _featureCollection :: Text
_point = "Point"
_multiPoint = "MultiPoint"
_lineString = "LineString"
_polygon = "Polygon"
_multiLineString = "MultiLineString"
_multiPolygon = "MultiPolygon"
_geometryCollection = "GeometryCollection"
_feature = "Feature"
_featureCollection = "FeatureCollection"




parserMaybe :: Maybe a -> Aeson.Parser a
parserMaybe = maybe empty return


_AesonBson :: Iso' Aeson.Value Bson.Value
_AesonBson = iso fromAeson toAeson

{-
instance Aeson.FromJSON Bson.Value where
  parseJSON = pure . view _AesonBson

instance Aeson.ToJSON Bson.Value where
  toJSON = review _AesonBson

instance Bson.Val Aeson.Value where
  val = view _AesonBson
  cast' = pure . review _AesonBson
-}


fromAeson :: Aeson.Value -> Bson.Value
fromAeson Aeson.Null = Bson.Null
fromAeson (Aeson.Bool a) = Bson.val a
fromAeson (Aeson.String a) = Bson.val a
fromAeson (Aeson.Array a) = Bson.val $ (fromAeson <$> a) ^.. traverse
fromAeson (Aeson.Object o ) = Bson.val .
  Map.foldrWithKey (\k v a -> (k := fromAeson v) : a ) mempty $ o
fromAeson (Aeson.Number a) = Bson.val $
  if isFloating a then
    -- TODO: check weather Double or Float
    Bson.val (toRealFloat a :: Double) else
    maybe (error $ "fromAeson: unable to cast " ++ show a ++ " to Integer") Bson.val
    -- TODO: we want an Integer
    (toBoundedInteger a :: Maybe Int)


toAeson :: Bson.Value -> Aeson.Value
toAeson (Bson.Float a)   = toJSON a
toAeson (Bson.String a)  = toJSON a
toAeson (Bson.Doc a)     =
  Aeson.object [ Bson.label f .=  toAeson (Bson.value f) | f <- a ]
toAeson (Bson.Array a)   = toJSON $ fmap toAeson a
toAeson (Bson.Bin a)     = toJSON . show $ a
toAeson (Bson.Fun a)     = toJSON . show $ a
toAeson (Bson.Uuid a)    = toJSON . show $ a
toAeson (Bson.Md5 a)     = toJSON . show $ a
toAeson (Bson.UserDef a) = toJSON . show $ a
toAeson (Bson.ObjId a)   = toJSON . show $ a
toAeson (Bson.Bool a)    = toJSON a
toAeson (Bson.UTC  a)    = toJSON a
toAeson Bson.Null        = Aeson.Null
toAeson (Bson.RegEx a)   = toJSON . show $ a
toAeson (Bson.JavaScr a) = toJSON . show $ a
toAeson (Bson.Sym  a)    = toJSON . show $ a
toAeson (Bson.Int32  a)  = toJSON a
toAeson (Bson.Int64  a)  = toJSON a
toAeson (Bson.Stamp  a)  = toJSON . show $ a
toAeson (Bson.MinMax a)  = toJSON . show $ a
