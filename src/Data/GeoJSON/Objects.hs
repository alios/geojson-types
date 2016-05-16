{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.GeoJSON.Objects
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
module Data.GeoJSON.Objects
       ( -- * GeoJSON Objects
         -- ** Position         
         Position, _Position,
         -- ** Point
         Point, _Point,
         -- ** MultiPoint
         MultiPoint, _MultiPoint,
         -- ** Line String         
         LineString, _LineString,
         -- ** Linear Ring 
         LinearRing, _LinearRing, closeLineString,
         -- ** MultiLineString
         MultiLineString, _MultiLineString,
         -- ** Polygon 
         Polygon, _Polygon,
         -- ** MultiPolygon
         MultiPolygon, _MultiPolygon,
         -- ** Collection
         Collection, _GeometryCollection,
         -- * Geometry Collection
         GeometryCollection, newCollection, insert,
         -- * Support types
         HasFlatCoordinates(..), boundingBox,
         GeoJSON, BaseType, GeoJSONObject
       ) where

import qualified Data.Text as T
import Control.Lens.Fold
import Control.Lens.Review
import Control.Lens.Prism
import Control.Lens.Iso
import Control.Lens.Getter
import Data.Maybe (catMaybes, fromMaybe)
import Data.Typeable (Typeable)
import Data.Aeson (toJSON, parseJSON, (.=), (.:))
import qualified Data.Aeson.Types as Aeson
import Data.Bson (Field(..), cast', val)
import qualified Data.Bson as Bson
import Control.Monad
import Data.GeoJSON.Intern

--
-- BaseType
--

-- | type constraint for the base numeric type used in 'Position'
type BaseType t =
  (Eq t, Ord t, Num t, Show t, Aeson.FromJSON t, Aeson.ToJSON t, Bson.Val t)

--
-- Bounding Box
--

-- | A bounding box is represented by a top-left/bottom-right
--   'Position' pair.
type BoundingBox t = (Position t, Position t)

-- | Represents datatypes which hold one or more 'Position' objects.
class BaseType t => HasFlatCoordinates a t | a -> t where
  flatCoordinates :: Getter a [Position t]

-- | calculate the bounding box of the the given object.
boundingBox :: (HasFlatCoordinates a t) => Getter a (BoundingBox t)
boundingBox = to $ calcBbox . view flatCoordinates
  
--
-- Position
--

-- | Data type to hold a basic x/y lat/lon value.
--   .
--   see also: <http://geojson.org/geojson-spec.html#positions> 
data Position t where
  Position :: BaseType t => (t, t) -> Position t

-- | 'Iso' from/to 'Position'
_Position :: BaseType t => Iso' (t, t) (Position t) 
_Position = iso toPos fromPos 
  where fromPos :: BaseType t => Position t -> (t, t)
        fromPos (Position p) = p        
        toPos (lat, lon ) = Position (lat, lon)

instance (Eq t) => Eq (Position t) where
  (Position a) == (Position b) = a == b

instance BaseType t => Show (Position t) where
  show = show . toJSON

instance BaseType t => Aeson.ToJSON (Position t) where
  toJSON = toJSON . view (from _Position)

instance BaseType t => Aeson.FromJSON (Position t) where
  parseJSON = fmap (view  _Position) . parseJSON


instance BaseType t => Bson.Val (Position t) where
  val a = let (lat, lon) = view (from _Position) a
          in val [lat,lon]
  cast' (Bson.Array [lat', lon']) = do
    ll <- (,) <$> cast' lat' <*> cast' lon'    
    return $ ll ^. _Position
  cast' _ = Nothing

instance (BaseType t) => HasFlatCoordinates (Position t) t where
  flatCoordinates = to pure


--
-- GeoJSON Objects
--


-- | see also: <http://geojson.org/geojson-spec.html#point>
data Point

_Point :: Iso' (Position t) (GeoJSON Point t)
_Point = iso Point (\(Point p) -> p)


-- | see also: <http://geojson.org/geojson-spec.html#multipoint>
data MultiPoint

_MultiPoint :: Iso' [Position t] (GeoJSON MultiPoint t)
_MultiPoint = iso MultiPoint (\(MultiPoint ps) -> ps)


-- | see also: <http://geojson.org/geojson-spec.html#linestring>
data LineString

_LineString :: Prism' [Position t] (GeoJSON LineString t)
_LineString = prism' (\(LineString ps) -> ps) toLS
  where toLS ls@(_ : _ : _) = pure $ LineString ls
        toLS _ = Nothing


-- | see also: <http://geojson.org/geojson-spec.html#linestring>
data LinearRing

_LinearRing :: BaseType t => Prism' (GeoJSON LineString t) (GeoJSON LinearRing t)
_LinearRing = prism' lrTols lsTolr 
  where lrTols :: GeoJSON LinearRing t -> GeoJSON LineString t
        lrTols (LinearRing ls) = ls
        lsTolr :: BaseType t => GeoJSON LineString t -> Maybe (GeoJSON LinearRing t)
        lsTolr ls =
          let ps = review _LineString ls
          in if (head ps == last ps) && (length ps >= 4)
             then pure $ LinearRing ls
             else Nothing

-- | create 'LinearRing' from 'LineString' by closing it.
closeLineString ::
  BaseType t => GeoJSON LineString t -> Maybe (GeoJSON LinearRing t)
closeLineString ls =
  let ps = review _LineString ls
  in if head ps == last ps
     then preview _LinearRing ls
     else preview _LineString (last ps : ps) >>= preview _LinearRing


-- | see also: http://geojson.org/geojson-spec.html#multilinestring
data MultiLineString

_MultiLineString :: Iso' [GeoJSON LineString t] (GeoJSON MultiLineString t)
_MultiLineString = iso MultiLineString (\(MultiLineString lss) -> lss)


-- | see also: http://geojson.org/geojson-spec.html#polygon
data Polygon

_Polygon :: Iso' [GeoJSON LinearRing t] (GeoJSON Polygon t)
_Polygon = iso Polygon (\(Polygon lr) -> lr)

-- | see also: http://geojson.org/geojson-spec.html#multipolygon
data MultiPolygon

_MultiPolygon :: Iso' [GeoJSON Polygon t] (GeoJSON MultiPolygon t)
_MultiPolygon = iso MultiPolygon (\(MultiPolygon lr) -> lr)

-- | see also: http://geojson.org/geojson-spec.html#geometry-collection
data Collection

_GeometryCollection :: Iso' (GeometryCollection t) (GeoJSON Collection t)
_GeometryCollection = iso GeometryCollection (\(GeometryCollection t) -> t)


--
-- GeoJSON
--

data GeoJSON a t where
  Point :: Position t -> GeoJSON Point t
  MultiPoint :: [Position t] -> GeoJSON MultiPoint t
  LineString :: [Position t] -> GeoJSON LineString t
  LinearRing :: GeoJSON LineString t -> GeoJSON LinearRing t
  MultiLineString :: [GeoJSON LineString t] -> GeoJSON MultiLineString t
  Polygon :: [GeoJSON LinearRing t] -> GeoJSON Polygon t
  MultiPolygon :: [GeoJSON Polygon t] -> GeoJSON MultiPolygon t
  GeometryCollection :: GeometryCollection t -> GeoJSON Collection t
  deriving (Typeable)

instance (GeoJSONObject a, BaseType t) => Eq (GeoJSON a t) where
  a == b = toJSON a == toJSON b

instance (GeoJSONObject a, BaseType t) => Show (GeoJSON a t) where
  show = show . toJSON

instance (GeoJSONObject a, BaseType t) => Aeson.ToJSON (GeoJSON a t) where
  toJSON p@(Point _) = mkObject pointT p
  toJSON p@(MultiPoint _) = mkObject multiPointT p
  toJSON p@(LineString _) = mkObject lineStringT p
  toJSON p@(LinearRing _) = mkObject linearRingT p
  toJSON p@(MultiLineString _) = mkObject multiLineStringT p
  toJSON p@(Polygon _) = mkObject polygonT p
  toJSON p@(MultiPolygon _) = mkObject multiPolygonT p
  toJSON p@(GeometryCollection c) = toJSON c

instance (GeoJSONObject a, BaseType t) => Aeson.FromJSON (GeoJSON a t) where
  parseJSON = parseGeoJSON

instance (GeoJSONObject a, BaseType t) => Bson.Val (GeoJSON a t) where
  val p@(Point _) = mkBsonObject pointT p
  val p@(MultiPoint _) = mkBsonObject multiPointT p
  val p@(LineString _) = mkBsonObject lineStringT p
  val p@(LinearRing _) = mkBsonObject linearRingT p
  val p@(MultiLineString _) = mkBsonObject multiLineStringT p
  val p@(Polygon _) = mkBsonObject polygonT p
  val p@(MultiPolygon _) = mkBsonObject multiPolygonT p
  val p@(GeometryCollection c) = val c
  cast' = castBson

instance (GeoJSONObject a, BaseType t) => HasFlatCoordinates (GeoJSON a t) t where
  flatCoordinates = flatCoordinatesGeoJSON


--
-- GeometryCollection
--

data GeometryCollection t where
  GCZero  :: GeometryCollection t
  GCCons  :: GeoJSONObject a =>
              GeoJSON a t -> GeometryCollection t -> GeometryCollection t
  deriving (Typeable)

newCollection :: 
  (GeoJSONObject a) => GeoJSON a t -> GeometryCollection t
newCollection = insert GCZero

insert ::
  (GeoJSONObject a) => GeometryCollection t ->  GeoJSON a t -> GeometryCollection t
insert = flip GCCons

instance BaseType t => Eq (GeometryCollection t) where
  a == b = toJSON a == toJSON b

instance BaseType t => Show (GeometryCollection t) where
  show = show . toJSON

instance (BaseType t) => Aeson.ToJSON (GeometryCollection t) where
  toJSON a = Aeson.object 
    [typeT .= geometryCollectionT, T.pack geometriesT .= toValue a]
    where
      toValue GCZero = []
      toValue (GCCons a as) = toJSON a : toValue as

instance (BaseType t) => Aeson.FromJSON (GeometryCollection t) where
  parseJSON = Aeson.withObject geometryCollectionT $ \o -> do
    t <- o .: typeT
    if t /= geometryCollectionT
      then fail $ "unable read type : " ++ geometryCollectionT
      else withNamedArray geometriesT o foldCollectionJSON

instance (BaseType t) => Bson.Val (GeometryCollection t) where
  val a = Bson.Doc
    [ typeT := val geometryCollectionT
    , T.pack geometriesT := Bson.Array (toValue a)
    ]
    where
      toValue GCZero = []
      toValue (GCCons a as) = val a : toValue as
  cast' (Bson.Doc d) = do
    t <- Bson.lookup typeT d
    if t /= geometryCollectionT then Nothing
      else do
        a <- Bson.look (T.pack geometriesT) d
        case a of
          Bson.Array a' -> foldCollectionBSON a'
          _ -> Nothing
  cast' _ = Nothing
  
--
-- GeoJSONObject
--
class (Typeable a) => GeoJSONObject a where
  type GeoJSONObjectType a t :: *
  _GeoObject :: BaseType t => Prism' (GeoJSONObjectType a t) (GeoJSON a t)
  parseGeoJSON :: BaseType t => Aeson.Value -> Aeson.Parser (GeoJSON a t)
  castBson :: BaseType t => Bson.Value -> Maybe (GeoJSON a t)
  flatCoordinatesGeoJSON ::
    (BaseType t) => Getter (GeoJSON a t) [Position t]
  
instance GeoJSONObject Point where
  type GeoJSONObjectType Point t = Position t
  _GeoObject = prism' (view $ from _Point) (pure . view _Point)
  castBson = castGeoBSON pointT
  parseGeoJSON = parseGeoJSONbyName pointT
  flatCoordinatesGeoJSON = to $ pure . view (from _Point)
  
instance GeoJSONObject MultiPoint where
  type GeoJSONObjectType MultiPoint t = [Position t]
  _GeoObject = prism' (view $ from _MultiPoint) (pure . view _MultiPoint)
  castBson = castGeoBSON multiPointT
  parseGeoJSON = parseGeoJSONbyName multiPointT
  flatCoordinatesGeoJSON = to $ view (from _MultiPoint)
                               
instance GeoJSONObject LineString where
  type GeoJSONObjectType LineString t = [Position t]
  _GeoObject = prism' (review _LineString) (preview _LineString)
  castBson = castGeoBSON lineStringT
  parseGeoJSON = parseGeoJSONbyName lineStringT
  flatCoordinatesGeoJSON = to $ review _LineString

instance GeoJSONObject LinearRing where
  type GeoJSONObjectType LinearRing t = [Position t]
  _GeoObject = prism'
    (review _GeoObject . review _LinearRing)
    (preview _LineString >=> preview _LinearRing ) 
  castBson = castGeoBSON linearRingT
  parseGeoJSON = parseGeoJSONbyName linearRingT
  flatCoordinatesGeoJSON = to $ view flatCoordinatesGeoJSON . review _LinearRing
    
instance GeoJSONObject MultiLineString where
  type GeoJSONObjectType MultiLineString t = [[Position t]]
  _GeoObject = prism'
    (traverseObjectsWithIso _MultiLineString)
    (traverseGeoObjectsWithGetter _MultiLineString )
  castBson = castGeoBSON multiLineStringT
  parseGeoJSON = parseGeoJSONbyName multiLineStringT
  flatCoordinatesGeoJSON = to $
    mconcat . fmap (view flatCoordinatesGeoJSON) . view (from _MultiLineString)
    
instance GeoJSONObject Polygon where
  type GeoJSONObjectType Polygon t = [[Position t]]
  _GeoObject = prism'
    (traverseObjectsWithIso _Polygon)
    (traverseGeoObjectsWithGetter _Polygon )
  castBson = castGeoBSON polygonT
  parseGeoJSON = parseGeoJSONbyName polygonT
  flatCoordinatesGeoJSON = to $
    mconcat . fmap (view flatCoordinatesGeoJSON) . review _Polygon

instance GeoJSONObject MultiPolygon where
  type GeoJSONObjectType MultiPolygon t = [[[Position t]]]
  _GeoObject = prism'
    (traverseObjectsWithIso _MultiPolygon)
    (traverseGeoObjectsWithGetter _MultiPolygon )
  castBson =  castGeoBSON multiPolygonT
  parseGeoJSON = parseGeoJSONbyName multiPolygonT
  flatCoordinatesGeoJSON = to $
    mconcat . fmap (view flatCoordinatesGeoJSON) . view (from _MultiPolygon)
  
instance GeoJSONObject Collection where
  type GeoJSONObjectType Collection t = GeometryCollection t
  _GeoObject = prism' (view (from _GeometryCollection))
    (pure . view _GeometryCollection)
  castBson = fmap (view _GeometryCollection) . cast'
  parseGeoJSON = fmap (view _GeometryCollection) . parseJSON
  flatCoordinatesGeoJSON = to $ colFlatPs . view (from _GeometryCollection)
    where colFlatPs GCZero = mempty
          colFlatPs (GCCons x xs) =
            mappend (x ^. flatCoordinatesGeoJSON) (colFlatPs xs)
  
--
-- Helpers
--

calcBbox :: BaseType t => [Position t] -> BoundingBox t
calcBbox as =
  let (xs, ys) = unzip . fmap (view (from _Position)) $ as
      minc = (minimum xs, minimum ys) ^. _Position
      maxc = (maximum xs, maximum ys) ^. _Position
  in (minc, maxc)


mkObject :: (BaseType t, Aeson.ToJSON (GeoJSONObjectType a t), GeoJSONObject a) =>
       String -> GeoJSON a t -> Aeson.Value
mkObject t p = Aeson.object [typeT .= t, coordinatesT .= review _GeoObject p]

mkBsonObject :: (BaseType t, Bson.Val (GeoJSONObjectType a t), GeoJSONObject a) =>
       String -> GeoJSON a t -> Bson.Value
mkBsonObject t p = Bson.Doc
  [ typeT := val t
  , coordinatesT := val (review _GeoObject p)
  ]


foldCollectionJSON ::
  (BaseType t, Functor f, Foldable f, Traversable f, Monad m) =>
  f Aeson.Value -> m (GeometryCollection t)
foldCollectionJSON a = case sequence . fmap parseGCCons $ a of
  Nothing -> fail "unable to read GeometryCollcetion elements"
  Just cs -> return . foldr ($) GCZero $ cs

foldCollectionBSON ::
  (BaseType t, Functor f, Foldable f, Traversable f, Monad m) =>
  f Bson.Value -> m (GeometryCollection t)
foldCollectionBSON a = case sequence . fmap castGCCons $ a of
  Nothing -> fail "unable to read GeometryCollcetion elements"
  Just cs -> return . foldr ($) GCZero $ cs
  
parseGCCons ::
  BaseType t => Aeson.Value -> Maybe (GeometryCollection t -> GeometryCollection t)
parseGCCons v = case parseGCCons' v of
  (cc:_) -> pure cc
  _ -> Nothing

castGCCons ::
  BaseType t => Bson.Value -> Maybe (GeometryCollection t -> GeometryCollection t)
castGCCons v = case castGCCons' v of
  (cc:_) -> pure cc
  _ -> Nothing
  
parseGeoJSONbyName ::
  (GeoJSONObject a, Aeson.FromJSON (GeoJSONObjectType a t), BaseType t) =>
  String -> Aeson.Value -> Aeson.Parser (GeoJSON a t)
parseGeoJSONbyName n =  Aeson.withObject pointT $ \o -> do
  t <- (o .: typeT) :: Aeson.Parser String
  if t /= n then fail $ "unable to parse type " ++ n
    else (o .: coordinatesT) >>= 
         maybe (fail $ "unable to parse coordinates of " ++ n) return  .
         preview _GeoObject 

castGeoBSON ::
  (GeoJSONObject a, Bson.Val (GeoJSONObjectType a t), BaseType t) =>
  String -> Bson.Value -> Maybe (GeoJSON a t)
castGeoBSON n (Bson.Doc o) = do
  t <- Bson.lookup typeT o
  if t /= n then fail $ "unable to parse type " ++ n
    else do
      cs <- Bson.lookup coordinatesT o
      preview _GeoObject cs
castGeoBSON _ _ = Nothing

parseGCCons' ::
  (BaseType t) => Aeson.Value -> [GeometryCollection t -> GeometryCollection t]
parseGCCons' v = catMaybes [Aeson.parseMaybe p v | p <- ps]
  where ps =
          [ fmap GCCons . parsePoint
          , fmap GCCons . parseMultiPoint            
          , fmap GCCons . parseLineString
          , fmap GCCons . parseLinearRing
          , fmap GCCons . parseMultiLineString            
          , fmap GCCons . parsePolygon
          , fmap GCCons . parseMultiPolygon
          , fmap GCCons . parseCollection
          ]
        parsePoint ::
          BaseType t => Aeson.Value -> Aeson.Parser (GeoJSON Point t)
        parsePoint = parseGeoJSON
        parseMultiPoint ::
          BaseType t => Aeson.Value -> Aeson.Parser (GeoJSON MultiPoint t)
        parseMultiPoint = parseGeoJSON
        parseLineString ::
          BaseType t => Aeson.Value -> Aeson.Parser (GeoJSON LineString t)
        parseLineString = parseGeoJSON
        parseLinearRing ::
          BaseType t => Aeson.Value -> Aeson.Parser (GeoJSON LinearRing t)
        parseLinearRing = parseGeoJSON
        parseMultiLineString ::
          BaseType t => Aeson.Value -> Aeson.Parser (GeoJSON MultiLineString t)
        parseMultiLineString = parseGeoJSON
        parsePolygon ::
          BaseType t => Aeson.Value -> Aeson.Parser (GeoJSON Polygon t)
        parsePolygon = parseGeoJSON
        parseMultiPolygon ::
          BaseType t => Aeson.Value -> Aeson.Parser (GeoJSON MultiPolygon t)
        parseMultiPolygon =  parseGeoJSON
        parseCollection ::
          BaseType t => Aeson.Value -> Aeson.Parser (GeoJSON Collection t)
        parseCollection a = view _GeometryCollection <$> parseJSON a


castGCCons' ::
  (BaseType t) => Bson.Value -> [GeometryCollection t -> GeometryCollection t]
castGCCons' v = catMaybes [p v | p <- ps]
  where ps =
          [ fmap GCCons . castPoint
          , fmap GCCons . castMultiPoint            
          , fmap GCCons . castLineString
          , fmap GCCons . castLinearRing
          , fmap GCCons . castMultiLineString            
          , fmap GCCons . castPolygon
          , fmap GCCons . castMultiPolygon
          , fmap GCCons . castCollection
          ]
        castPoint ::
          BaseType t => Bson.Value -> Maybe (GeoJSON Point t)
        castPoint = castBson
        castMultiPoint ::
          BaseType t => Bson.Value -> Maybe (GeoJSON MultiPoint t)
        castMultiPoint = castBson
        castLineString ::
          BaseType t => Bson.Value -> Maybe (GeoJSON LineString t)
        castLineString = castBson
        castLinearRing ::
          BaseType t => Bson.Value -> Maybe (GeoJSON LinearRing t)
        castLinearRing = castBson
        castMultiLineString ::
          BaseType t => Bson.Value -> Maybe (GeoJSON MultiLineString t)
        castMultiLineString = castBson
        castPolygon ::
          BaseType t => Bson.Value -> Maybe (GeoJSON Polygon t)
        castPolygon = castBson
        castMultiPolygon ::
          BaseType t => Bson.Value -> Maybe (GeoJSON MultiPolygon t)
        castMultiPolygon =  castBson
        castCollection ::
          BaseType t => Bson.Value -> Maybe (GeoJSON Collection t)
        castCollection a = view _GeometryCollection <$> cast' a

traverseObjectsWithIso ::
  (BaseType t, GeoJSONObject b) =>
  Iso' [GeoJSON b t] (GeoJSON a t) -> GeoJSON a t -> [GeoJSONObjectType b t]
traverseObjectsWithIso i = fmap (review _GeoObject) <$> view (from i)

traverseGeoObjects ::
  (BaseType t, GeoJSONObject a, Traversable tt) =>
  tt (GeoJSONObjectType a t) -> Maybe (tt (GeoJSON a t))
traverseGeoObjects = sequence . fmap (preview _GeoObject)

traverseGeoObjectsWithGetter ::
  (BaseType t, Traversable tt, GeoJSONObject a) =>
  Getting b (tt (GeoJSON a t)) b ->
  tt (GeoJSONObjectType a t) ->
  Maybe b
traverseGeoObjectsWithGetter g = fmap (view g) . traverseGeoObjects 

