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
         Position, _Position, mapPosition,
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
         GeometryCollection, gcNew, gcInsert, gcMap,
         -- * Support types
         -- ** Bounding Box
         HasFlatCoordinates(..), BoundingBox, boundingBox,
         -- ** Base Types
         GeoJSON (..), mapGeoJSON,
         BaseType, GeoJSONObject(..), BaseGeoJSONObject
       ) where



import qualified Data.Text as T
import Control.Lens.Fold
import Control.Lens.Review
import Control.Lens.Prism
import Control.Lens.Iso
import Control.Lens.Getter
import Data.Maybe (catMaybes)
import Data.Typeable (Typeable)
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Control.Monad
import Data.GeoJSON.Intern
import Database.Persist
import Database.Persist.Sql
import qualified Data.Bson as Bson
import Data.Vector (Vector)
import qualified Data.Vector as V

--
-- BaseType
--

-- | type constraint for the base numeric type used in 'Position'
type BaseType t =
  (Typeable t, Eq t, Ord t, Num t, Show t, Aeson.FromJSON t, Aeson.ToJSON t)

type BaseGeoJSONObject a t = (GeoJSONObject a, BaseType t)

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
boundingBox = flatCoordinates . to calcBbox


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
  show = showJSON

instance BaseType t => Aeson.ToJSON (Position t) where
  toJSON = toJSON . view (from _Position)

instance BaseType t => Aeson.FromJSON (Position t) where
  parseJSON = fmap (view  _Position) . parseJSON

mapPosition :: (BaseType a, BaseType b) => (a -> b) -> Position a -> Position b
mapPosition f (Position (a, b)) = Position (f a, f b)

bimapPosition :: (BaseType t) =>
                 (t -> t -> t) -> Position t -> Position t -> Position t
bimapPosition f (Position (ax, ay)) (Position (bx, by)) =
  Position (f ax bx, f ay by)

instance BaseType t => PersistField (Position t) where
  toPersistValue = jsonToPersistValue
  fromPersistValue = persistFieldToValue

instance BaseType t => PersistFieldSql (Position t) where
  sqlType _ = SqlString

instance BaseType t => Bson.Val (Position t) where
  val = jsonToBson
  cast' = jsonFromBson

instance (BaseType t) => HasFlatCoordinates (Position t) t where
  flatCoordinates = to pure


instance (BaseType t) => Num (Position t) where
  (+) = bimapPosition (+)
  (-) = bimapPosition (-)
  (*) = bimapPosition (*)
  abs = mapPosition abs
  signum = mapPosition signum
  fromInteger i = Position (fromInteger i, 0)


--
-- GeoJSON Objects
--


-- | see also: <http://geojson.org/geojson-spec.html#point>
data Point

-- | convert from/to 'Point'
_Point :: Iso' (Position t) (GeoJSON Point t)
_Point = iso Point (\(Point p) -> p)


-- | see also: <http://geojson.org/geojson-spec.html#multipoint>
data MultiPoint

-- | convert from/to 'MultiPoint'
_MultiPoint :: Iso' [Position t] (GeoJSON MultiPoint t)
_MultiPoint = iso (MultiPoint . V.fromList) (\(MultiPoint ps) -> V.toList ps)

-- | see also: <http://geojson.org/geojson-spec.html#linestring>
data LineString

-- | convert from/to 'LineString'. Must have 2 or more elements.
_LineString :: Prism' [Position t] (GeoJSON LineString t)
_LineString = prism' (\(LineString ps) -> V.toList ps) toLS
  where toLS ls@(_ : _ : _) = pure . LineString . V.fromList $ ls
        toLS _ = Nothing

-- | see also: <http://geojson.org/geojson-spec.html#linestring>
data LinearRing

-- | a closed (first elemet == last element) 'LineString'. Must have
--   at least 4 elements.
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

-- | convert from/to 'MultiLineString'
_MultiLineString :: Iso' [GeoJSON LineString t] (GeoJSON MultiLineString t)
_MultiLineString =
  iso (MultiLineString . V.fromList) (\(MultiLineString lss) -> V.toList lss)


-- | see also: http://geojson.org/geojson-spec.html#polygon
data Polygon

-- | convert from/to 'Polygon'
_Polygon :: Iso' [GeoJSON LinearRing t] (GeoJSON Polygon t)
_Polygon = iso (Polygon . V.fromList) (\(Polygon lr) -> V.toList lr)

-- | see also: http://geojson.org/geojson-spec.html#multipolygon
data MultiPolygon

-- | convert from/to 'MultiPolygon'
_MultiPolygon :: Iso' [GeoJSON Polygon t] (GeoJSON MultiPolygon t)
_MultiPolygon = iso (MultiPolygon . V.fromList) (\(MultiPolygon lr) -> V.toList lr)

-- | see also: http://geojson.org/geojson-spec.html#geometry-collection
data Collection

-- | convert from/to 'GeometryCollection'
_GeometryCollection :: Iso' (GeometryCollection t) (GeoJSON Collection t)
_GeometryCollection = iso GeometryCollection (\(GeometryCollection t) -> t)


--
-- GeoJSON
--

-- | the base type of all GeoJSON object. see also 'GeoJSONObject'
data GeoJSON a t where
  Point :: Position t -> GeoJSON Point t
  MultiPoint :: Vector (Position t) -> GeoJSON MultiPoint t
  LineString :: Vector (Position t) -> GeoJSON LineString t
  LinearRing :: GeoJSON LineString t -> GeoJSON LinearRing t
  MultiLineString :: Vector (GeoJSON LineString t) -> GeoJSON MultiLineString t
  Polygon :: Vector (GeoJSON LinearRing t) -> GeoJSON Polygon t
  MultiPolygon :: Vector (GeoJSON Polygon t) -> GeoJSON MultiPolygon t
  GeometryCollection :: GeometryCollection t -> GeoJSON Collection t
  deriving (Typeable)

instance Monoid (GeoJSON Collection t) where
  mempty = GeometryCollection mempty
  mappend (GeometryCollection a) (GeometryCollection b) =
    GeometryCollection $ mappend a b

mapGeoJSON ::
  (BaseGeoJSONObject a t, BaseType s) => (t -> s) -> GeoJSON a t -> GeoJSON a s
mapGeoJSON f (Point p) = Point (mapPosition f p)
mapGeoJSON f (MultiPoint ps) = MultiPoint $ mapPosition f <$> ps
mapGeoJSON f (LineString ps) = LineString $ mapPosition f <$> ps
mapGeoJSON f (LinearRing ls) = LinearRing $ mapGeoJSON f ls
mapGeoJSON f (MultiLineString lrs) = MultiLineString $ mapGeoJSON f <$> lrs
mapGeoJSON f (Polygon lrs) = Polygon $ mapGeoJSON f <$> lrs
mapGeoJSON f (MultiPolygon ps) = MultiPolygon $ mapGeoJSON f <$> ps
mapGeoJSON f (GeometryCollection gc) = GeometryCollection $ gcMap f gc



instance (BaseGeoJSONObject a t) => Eq (GeoJSON a t) where
  a == b = toJSON a == toJSON b

instance (BaseGeoJSONObject a t) => Show (GeoJSON a t) where
  show = showJSON

instance (BaseGeoJSONObject a t) => Aeson.ToJSON (GeoJSON a t) where
  toJSON p@(Point _) = mkObject pointT p
  toJSON p@(MultiPoint _) = mkObject multiPointT p
  toJSON p@(LineString _) = mkObject lineStringT p
  toJSON p@(LinearRing _) = mkObject linearRingT p
  toJSON p@(MultiLineString _) = mkObject multiLineStringT p
  toJSON p@(Polygon _) = mkObject polygonT p
  toJSON p@(MultiPolygon _) = mkObject multiPolygonT p
  toJSON (GeometryCollection c) = toJSON c

instance (GeoJSONObject a, BaseType t) => Aeson.FromJSON (GeoJSON a t) where
  parseJSON = parseGeoJSON

instance (GeoJSONObject a, BaseType t) => HasFlatCoordinates (GeoJSON a t) t where
  flatCoordinates = flatCoordinatesGeoJSON

instance (GeoJSONObject a, BaseType t) => PersistField (GeoJSON a t) where
  toPersistValue = jsonToPersistValue
  fromPersistValue = persistFieldToValue

instance (GeoJSONObject a, BaseType t) => PersistFieldSql (GeoJSON a t) where
  sqlType _ = SqlString

instance (GeoJSONObject a, BaseType t) => Bson.Val (GeoJSON a t) where
  val = jsonToBson
  cast' = jsonFromBson


--
-- GeometryCollection
--

-- | a collection of 'GeoJSONObject'.
data GeometryCollection t where
  GCZero  :: GeometryCollection t
  GCCons  :: GeoJSONObject a =>
              GeoJSON a t -> GeometryCollection t -> GeometryCollection t
  deriving (Typeable)


instance Monoid (GeometryCollection t) where
  mempty = GCZero
  mappend GCZero  GCZero = GCZero
  mappend GCZero a@(GCCons _ _) = a
  mappend a@(GCCons _ _) GCZero = a
  mappend a@(GCCons _ _) (GCCons bv bs) = mappend (GCCons bv a) bs

-- | create a new 'GeometryCollection' with initial element
gcNew :: (GeoJSONObject a) => GeoJSON a t -> GeometryCollection t
gcNew = gcInsert GCZero

-- | insert a 'GeoJSONObject' into 'GeometryCollection'
gcInsert ::
  (GeoJSONObject a) => GeometryCollection t ->  GeoJSON a t -> GeometryCollection t
gcInsert = flip GCCons

gcMap :: (BaseType t, BaseType s) =>
         (t -> s) -> GeometryCollection t -> GeometryCollection s
gcMap _ GCZero = GCZero
gcMap f (GCCons x xs) = GCCons (mapGeoJSON f x) $ gcMap f xs

instance BaseType t => Eq (GeometryCollection t) where
  a == b = toJSON a == toJSON b

instance BaseType t => Show (GeometryCollection t) where
  show = showJSON

instance (BaseType t) => Aeson.ToJSON (GeometryCollection t) where
  toJSON a = Aeson.object
    [typeT .= geometryCollectionT, T.pack geometriesT .= toValue a]
    where
      toValue GCZero = []
      toValue (GCCons a' as) = toJSON a' : toValue as

instance (BaseType t) => Aeson.FromJSON (GeometryCollection t) where
  parseJSON = Aeson.withObject geometryCollectionT $ \o -> do
    t <- o .: typeT
    if t /= geometryCollectionT
      then fail $ "unable read type : " ++ geometryCollectionT
      else withNamedArray geometriesT o foldCollectionJSON

instance BaseType t => PersistField (GeometryCollection t) where
  toPersistValue = jsonToPersistValue
  fromPersistValue = persistFieldToValue

instance BaseType t => PersistFieldSql (GeometryCollection t) where
  sqlType _ = SqlString

instance BaseType t => Bson.Val (GeometryCollection t) where
  val = jsonToBson
  cast' = jsonFromBson

--
-- GeoJSONObject
--

-- | common type clas of all 'GeoJSON' objects
class (Typeable a) => GeoJSONObject a where
  type GeoJSONObjectType a t :: *
  _GeoObject :: BaseType t => Prism' (GeoJSONObjectType a t) (GeoJSON a t)
  parseGeoJSON :: BaseType t => Aeson.Value -> Aeson.Parser (GeoJSON a t)
  fromPersist :: BaseType t => PersistValue -> Maybe (GeoJSON a t)
  flatCoordinatesGeoJSON ::
    (BaseType t) => Getter (GeoJSON a t) [Position t]

instance GeoJSONObject Point where
  type GeoJSONObjectType Point t = Position t
  _GeoObject = prism' (view $ from _Point) (pure . view _Point)
  parseGeoJSON = parseGeoJSONbyName pointT
  flatCoordinatesGeoJSON = from _Point . to pure
  fromPersist = toGeoPersist pointT

instance GeoJSONObject MultiPoint where
  type GeoJSONObjectType MultiPoint t = [Position t]
  _GeoObject = prism' (view $ from _MultiPoint) (pure . view _MultiPoint)
  parseGeoJSON = parseGeoJSONbyName multiPointT
  fromPersist = toGeoPersist multiPointT
  flatCoordinatesGeoJSON = from _MultiPoint

instance GeoJSONObject LineString where
  type GeoJSONObjectType LineString t = [Position t]
  _GeoObject = prism' (review _LineString) (preview _LineString)
  parseGeoJSON = parseGeoJSONbyName lineStringT
  fromPersist = toGeoPersist lineStringT
  flatCoordinatesGeoJSON = re _LineString

instance GeoJSONObject LinearRing where
  type GeoJSONObjectType LinearRing t = [Position t]
  _GeoObject = prism'
    (review _GeoObject . review _LinearRing)
    (preview _LineString >=> preview _LinearRing )
  parseGeoJSON = parseGeoJSONbyName linearRingT
  fromPersist = toGeoPersist linearRingT
  flatCoordinatesGeoJSON = re _LinearRing . flatCoordinatesGeoJSON

instance GeoJSONObject MultiLineString where
  type GeoJSONObjectType MultiLineString t = [[Position t]]
  _GeoObject = prism'
    (traverseObjectsWithIso _MultiLineString)
    (traverseGeoObjectsWithGetter _MultiLineString )
  parseGeoJSON = parseGeoJSONbyName multiLineStringT
  fromPersist = toGeoPersist multiLineStringT
  flatCoordinatesGeoJSON =
    flatCoordinatesList (from _MultiLineString)

instance GeoJSONObject Polygon where
  type GeoJSONObjectType Polygon t = [[Position t]]
  _GeoObject = prism'
    (traverseObjectsWithIso _Polygon)
    (traverseGeoObjectsWithGetter _Polygon )
  parseGeoJSON = parseGeoJSONbyName polygonT
  fromPersist = toGeoPersist polygonT
  flatCoordinatesGeoJSON = flatCoordinatesList (re _Polygon)

instance GeoJSONObject MultiPolygon where
  type GeoJSONObjectType MultiPolygon t = [[[Position t]]]
  _GeoObject = prism'
    (traverseObjectsWithIso _MultiPolygon)
    (traverseGeoObjectsWithGetter _MultiPolygon )
  parseGeoJSON = parseGeoJSONbyName multiPolygonT
  fromPersist = toGeoPersist multiPolygonT
  flatCoordinatesGeoJSON = flatCoordinatesList (re _MultiPolygon)

instance GeoJSONObject Collection where
  type GeoJSONObjectType Collection t = GeometryCollection t
  _GeoObject = prism'
    (view (from _GeometryCollection))
    (pure . view _GeometryCollection)
  parseGeoJSON = fmap (view _GeometryCollection) . parseJSON
  fromPersist = fmap (view _GeometryCollection) .
    either (const Nothing) pure . fromPersistValue
  flatCoordinatesGeoJSON =  re _GeometryCollection . to colFlatPs
    where colFlatPs GCZero = mempty
          colFlatPs (GCCons x xs) =
            mappend (x ^. flatCoordinatesGeoJSON) (colFlatPs xs)


--
-- Helpers
--

calcBbox :: BaseType t => [Position t] -> BoundingBox t
calcBbox as =
  let (xs, ys) = unzip . fmap (review _Position) $ as
      minc = (minimum xs, minimum ys) ^. _Position
      maxc = (maximum xs, maximum ys) ^. _Position
  in (minc, maxc)


mkObject :: (BaseType t, Aeson.ToJSON (GeoJSONObjectType a t), GeoJSONObject a) =>
       String -> GeoJSON a t -> Aeson.Value
mkObject t p = Aeson.object [typeT .= t, coordinatesT .= review _GeoObject p]


flatCoordinatesList ::
  (BaseType t, GeoJSONObject a) =>
  Getter b [GeoJSON a t] -> Getter b [Position t]
flatCoordinatesList ga = getterMapConcat ga flatCoordinatesGeoJSON

getterMapConcat ::
  (Monoid c) => Getter a [b] ->  Getter b c -> Getter a c
getterMapConcat ga gb = getterMap ga gb . to mconcat

getterMap :: (Functor f) => Getter a (f b) ->  Getter b c -> Getter a (f c)
getterMap ga gb = to $ \a -> view gb <$> a ^. ga


foldCollectionJSON ::
  (BaseType t, Functor f, Foldable f, Traversable f, Monad m) =>
  f Aeson.Value -> m (GeometryCollection t)
foldCollectionJSON a = case sequence . fmap parseGCCons $ a of
  Nothing -> fail "unable to read GeometryCollcetion elements"
  Just cs -> return . foldr ($) GCZero $ cs





parseGCCons ::
  BaseType t => Aeson.Value -> Maybe (GeometryCollection t -> GeometryCollection t)
parseGCCons v = case parseGCCons' v of
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


toGeoPersist ::
  (GeoJSONObject a, PersistField (GeoJSONObjectType a t), BaseType t) =>
  String -> PersistValue -> Maybe (GeoJSON a t)
toGeoPersist n (PersistMap m) = do
  t <- lookup typeT m >>=
    either (const Nothing) pure . fromPersistValue
  if t /= n then fail $ "unable to parse type " ++ n
    else do
    gs <- lookup coordinatesT m >>=
      either (const Nothing) pure . fromPersistValue
    preview _GeoObject gs
toGeoPersist _ _ = Nothing


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
