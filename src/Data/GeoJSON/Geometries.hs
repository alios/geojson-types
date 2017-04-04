{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.GeoJSON.Geometries
-- Copyright   :  (C) 2016 Markus Barenhoff
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Markus Barenhoff <mbarenh@alios.org>
-- Stability   :  provisional
-- Portability :  FlexibleInstances, MultiParamTypeClasses, TypeFamilies
--
----------------------------------------------------------------------------

module Data.GeoJSON.Geometries
  ( Point, _Point
  , MultiPoint, _MultiPoint, _MultiPoints
  , LineString, _LineString, _LineStringPoints
  , Polygon, _Polygon, _PolygonPoints
  , MultiLineString, _MultiLineString, _MultiLineStrings
  , MultiPolygon, _MultiPolygon, _MultiPolygons
  , GeometryObject, _GeometryObject
  , _PointObject, _MultiPointObject, _LineStringObject, _PolygonObject
  , _MultiLineStringObject, _MultiPolygonObject, _GeometryCollection
  ) where

import           Control.Applicative

import           Control.Lens.Iso
import           Control.Lens.Prism
import           Control.Lens.Review
import           Data.Aeson            as Aeson
import           Data.Aeson.Types      as Aeson
import           Data.GeoJSON.Classes
import           Data.GeoJSON.Intern
import           Data.GeoJSON.Position
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Text             (Text)
import           Data.Vector           (Vector)
import qualified Data.Vector           as V

class HasGeometryObject g where
  toGeometryObject :: Geometry g a -> GeometryObject a
  fromGeometryObject :: GeometryObject a -> Maybe (Geometry g a)

_GeometryObject :: HasGeometryObject g => Prism' (GeometryObject a) (Geometry g a)
_GeometryObject = prism' toGeometryObject fromGeometryObject

data Point
instance BaseType a => IsGeometry Point (Position a) a where
  data Geometry Point a = Point (Position a)
    deriving (Eq, Show)
  type GeometryStructure Point a fv fc = PositionStructure fc a
  _Geometry = iso Point (\(Point a) -> a)
  geometryType = const _point

_Point :: (PositionStructureClass fc a) =>
  Prism' (GeometryStructure Point a fv fc) (Geometry Point a)
_Point = _Position . _Geometry

instance HasGeometryObject Point where
  toGeometryObject = GeometryPoint
  fromGeometryObject (GeometryPoint a) = pure a
  fromGeometryObject _                 = empty

instance Functor (Geometry Point) where
  fmap f (Point a) = Point $ fmap f a

instance BaseType a => HasBoundingBox (Geometry Point a) a where
  boundingBox (Point a)   = boundingBox a


data MultiPoint
instance  BaseType a => IsGeometry MultiPoint (PositionVector a) a where
  data Geometry MultiPoint a = MultiPoint (PositionVector a)
    deriving (Eq, Show)
  type GeometryStructure MultiPoint a fv fc =
    PositionVectorStructure fv fc a
  _Geometry = iso MultiPoint (\(MultiPoint a) -> a)
  geometryType = const _multiPoint

_MultiPoint :: (VectorStructureClass fv fc a) =>
  Prism' (GeometryStructure MultiPoint a fv fc) (Geometry MultiPoint a)
_MultiPoint = _PositionVector . _Geometry

_MultiPoints ::
  (BaseType a, ListLike f (Geometry Point a)) =>
  Iso' (f (Geometry Point a)) (Geometry MultiPoint a)
_MultiPoints = iso
  (MultiPoint . foldMap (pure . review _Geometry))
  (foldMap (pure . Point)  . review _Geometry)


instance HasGeometryObject MultiPoint where
  toGeometryObject = GeometryMultiPoint
  fromGeometryObject (GeometryMultiPoint a) = pure a
  fromGeometryObject _                      = empty

instance Functor (Geometry MultiPoint) where
  fmap f (MultiPoint a) = MultiPoint $ fmap (fmap f) a

instance BaseType a => HasBoundingBox (Geometry MultiPoint a) a where
  boundingBox (MultiPoint a)   = foldMap boundingBox a


data LineString
instance BaseType a => IsGeometry LineString (PositionVector a) a where
  data Geometry LineString a = LineString (PositionVector a)
    deriving (Eq, Show)
  type GeometryStructure LineString a fv fc =
    PositionVectorStructure fv fc a
  _Geometry = iso LineString (\(LineString a) -> a)
  geometryType = const _lineString

_LineString :: (VectorStructureClass fv fc a) =>
  Prism' (GeometryStructure LineString a fv fc) (Geometry LineString a)
_LineString = _PositionVector . _Geometry

_LineStringPoints ::
  (BaseType a, ListLike f (Geometry Point a)) =>
  Iso' (f (Geometry Point a)) (Geometry LineString a)
_LineStringPoints = iso
  (LineString . foldMap (pure . review _Geometry))
  (foldMap (pure . Point)  . review _Geometry)

instance HasGeometryObject LineString where
  toGeometryObject = GeometryLineString
  fromGeometryObject (GeometryLineString a) = pure a
  fromGeometryObject _                      = empty

instance Functor (Geometry LineString) where
  fmap f (LineString a) = LineString $ fmap (fmap f) a

instance BaseType a => HasBoundingBox (Geometry LineString a) a where
  boundingBox (LineString a)   = foldMap boundingBox a

data Polygon
instance BaseType a => IsGeometry Polygon (PositionVector a) a where
  data Geometry Polygon a = Polygon (PositionVector a)
    deriving (Eq, Show)
  type GeometryStructure Polygon a fv fc =
    PositionVectorStructure fv fc a
  _Geometry = iso Polygon (\(Polygon a) -> a)
  geometryType = const _polygon

_Polygon :: (VectorStructureClass fv fc a) =>
  Prism' (GeometryStructure Polygon a fv fc) (Geometry Polygon a)
_Polygon = _PositionVector . _Geometry
-- TODO: check for closed ring


_PolygonPoints ::
  (BaseType a, ListLike f (Geometry Point a)) =>
  Prism' (f (Geometry Point a)) (Geometry Polygon a)
_PolygonPoints = prism'
  (foldMap (pure . Point)  . review _Geometry)
  (pure . Polygon . foldMap (pure . review _Geometry))
  -- TODO check for closed ring


instance HasGeometryObject Polygon where
  toGeometryObject = GeometryPolygon
  fromGeometryObject (GeometryPolygon a) = pure a
  fromGeometryObject _                   = empty

instance Functor (Geometry Polygon) where
  fmap f (Polygon a) = Polygon $ fmap (fmap f) a

instance BaseType a => HasBoundingBox (Geometry Polygon a) a where
  boundingBox (Polygon a)   = foldMap boundingBox a


data MultiLineString
instance BaseType a => IsGeometry MultiLineString (PositionVector2 a) a where
  data Geometry MultiLineString a = MultiLineString (PositionVector2 a)
    deriving (Eq, Show)
  type GeometryStructure MultiLineString a fv fc =
    PositionVector2Structure fv fc a
  _Geometry = iso MultiLineString (\(MultiLineString a) -> a)
  geometryType = const _multiLineString

_MultiLineString :: (VectorStructureClass fv fc a) =>
  Prism' (GeometryStructure MultiLineString a fv fc) (Geometry MultiLineString a)
_MultiLineString = _PositionVector2 . _Geometry


_MultiLineStrings ::
  (BaseType a, ListLike f (Geometry LineString a)) =>
  Iso' (f (Geometry LineString a)) (Geometry MultiLineString a)
_MultiLineStrings = iso
  (MultiLineString . foldMap (pure . review _Geometry))
  (foldMap (pure . LineString)  . review _Geometry)


instance HasGeometryObject MultiLineString where
  toGeometryObject = GeometryMultiLineString
  fromGeometryObject (GeometryMultiLineString a) = pure a
  fromGeometryObject _                           = empty

instance Functor (Geometry MultiLineString) where
  fmap f (MultiLineString a) = MultiLineString $ fmap (fmap (fmap f)) a

instance BaseType a => HasBoundingBox (Geometry MultiLineString a) a where
  boundingBox (MultiLineString a)   =
    mconcat (foldMap boundingBox <$> V.toList a)

data MultiPolygon
instance BaseType a => IsGeometry MultiPolygon (PositionVector2 a) a where
  data Geometry MultiPolygon a = MultiPolygon (PositionVector2 a)
    deriving (Eq, Show)
  type GeometryStructure MultiPolygon a fv fc =
    PositionVector2Structure fv fc a
  _Geometry = iso MultiPolygon (\(MultiPolygon a) -> a)
  geometryType = const _multiPolygon

_MultiPolygons ::
  (BaseType a, ListLike f (Geometry Polygon a)) =>
  Iso' (f (Geometry Polygon a)) (Geometry MultiPolygon a)

_MultiPolygons = iso
  (MultiPolygon . foldMap (pure . review _Geometry))
  (foldMap (pure . Polygon)  . review _Geometry)

instance HasGeometryObject MultiPolygon where
  toGeometryObject = GeometryMultiPolygon
  fromGeometryObject (GeometryMultiPolygon a) = pure a
  fromGeometryObject _                        = empty

instance Functor (Geometry MultiPolygon) where
  fmap f (MultiPolygon a) = MultiPolygon $ fmap (fmap (fmap f)) a

instance BaseType a => HasBoundingBox (Geometry MultiPolygon a) a where
  boundingBox (MultiPolygon a)   =
    mconcat (foldMap boundingBox <$> V.toList a)

_MultiPolygon :: (VectorStructureClass fv fc a) =>
  Prism' (GeometryStructure MultiPolygon a fv fc) (Geometry MultiPolygon a)
_MultiPolygon = _PositionVector2 . _Geometry
-- TODO: check for closed ring



--
-- Geometry Object
--

data GeometryObject a
  = GeometryPoint (Geometry Point a)
  | GeometryMultiPoint (Geometry MultiPoint a)
  | GeometryLineString (Geometry LineString a)
  | GeometryPolygon (Geometry Polygon a)
  | GeometryMultiLineString (Geometry MultiLineString a)
  | GeometryMultiPolygon (Geometry MultiPolygon a)
  | GeometryCollection (Vector (GeometryObject a))
  deriving (Eq, Show)


instance Functor GeometryObject where
  fmap f (GeometryPoint a)           = GeometryPoint $ fmap f a
  fmap f (GeometryMultiPoint a)      = GeometryMultiPoint $ fmap f a
  fmap f (GeometryLineString a)      = GeometryLineString $ fmap f a
  fmap f (GeometryPolygon a)         = GeometryPolygon $ fmap f a
  fmap f (GeometryMultiLineString a) = GeometryMultiLineString $ fmap f a
  fmap f (GeometryMultiPolygon a)    = GeometryMultiPolygon $ fmap f a
  fmap f (GeometryCollection a)      = GeometryCollection $ fmap (fmap f) a

instance BaseType a => HasBoundingBox (GeometryObject a) a where
  boundingBox (GeometryPoint a)           = boundingBox a
  boundingBox (GeometryMultiPoint a)      = boundingBox a
  boundingBox (GeometryLineString a)      = boundingBox a
  boundingBox (GeometryPolygon a)         = boundingBox a
  boundingBox (GeometryMultiLineString a) = boundingBox a
  boundingBox (GeometryMultiPolygon a)    = boundingBox a
  boundingBox (GeometryCollection a)      = foldMap boundingBox a

_PointObject :: (PositionStructureClass fc a) =>
  Prism' (GeometryStructure Point a fv fc) (GeometryObject a)
_PointObject = _Point . iso GeometryPoint (\(GeometryPoint a) -> a)

_MultiPointObject :: (VectorStructureClass fv fc a) =>
  Prism' (GeometryStructure MultiPoint a fv fc) (GeometryObject a)
_MultiPointObject = _MultiPoint .
  iso GeometryMultiPoint (\(GeometryMultiPoint a) -> a)

_LineStringObject :: (VectorStructureClass fv fc a) =>
  Prism' (GeometryStructure LineString a fv fc) (GeometryObject a)
_LineStringObject = _LineString .
  iso GeometryLineString (\(GeometryLineString a) -> a)

_PolygonObject :: (VectorStructureClass fv fc a) =>
  Prism' (GeometryStructure Polygon a fv fc) (GeometryObject a)
_PolygonObject = _Polygon .
  iso GeometryPolygon (\(GeometryPolygon a) -> a)

_MultiLineStringObject :: (VectorStructureClass fv fc a) =>
  Prism' (GeometryStructure MultiLineString a fv fc) (GeometryObject a)
_MultiLineStringObject = _MultiLineString .
  iso GeometryMultiLineString (\(GeometryMultiLineString a) -> a)

_MultiPolygonObject :: (VectorStructureClass fv fc a) =>
  Prism' (GeometryStructure MultiPolygon a fv fc) (GeometryObject a)
_MultiPolygonObject = _MultiPolygon .
  iso GeometryMultiPolygon (\(GeometryMultiPolygon a) -> a)

_GeometryCollection ::
  (BaseType a, ListLike f (GeometryObject a)) =>
  Prism' (GeometryObject a) (f (GeometryObject a))
_GeometryCollection = prism' f t
  where f = GeometryCollection . foldMap pure
        t (GeometryCollection a) = pure $ foldMap pure a
        t _                      = empty

instance BaseType a => ToJSON (GeometryObject a) where
  toJSON (GeometryPoint a)           = toJSON a
  toJSON (GeometryMultiPoint a)      = toJSON a
  toJSON (GeometryLineString a)      = toJSON a
  toJSON (GeometryPolygon a)         = toJSON a
  toJSON (GeometryMultiLineString a) = toJSON a
  toJSON (GeometryMultiPolygon a)    = toJSON a
  toJSON (GeometryCollection a)      = object
    [ typeField .= _geometryCollection
    , geometriesField .= toJSON a
    ]



objectTable :: BaseType a => Map Text (Value -> Parser (GeometryObject a))
objectTable = Map.fromList
  [ ( _point, fmap GeometryPoint . parseJSON)
  , ( _multiPoint, fmap GeometryMultiPoint . parseJSON)
  , ( _lineString, fmap GeometryLineString . parseJSON)
  , ( _polygon, fmap GeometryPolygon . parseJSON)
  , ( _multiLineString, fmap GeometryMultiLineString . parseJSON)
  , ( _multiPolygon, fmap GeometryMultiPolygon . parseJSON)
  , ( _geometryCollection, parseGeometryCollection)
  ]

parseGeometryCollection :: (BaseType a) => Value -> Parser (GeometryObject a)
parseGeometryCollection (Object a) = GeometryCollection <$> a .: geometriesField
parseGeometryCollection _ = empty

instance BaseType a => FromJSON (GeometryObject a) where
  parseJSON obj@(Object a) = do
    t <- (a .: typeField) :: Parser Text
    maybe empty (\f -> f obj) (Map.lookup t objectTable)
  parseJSON _ = empty
