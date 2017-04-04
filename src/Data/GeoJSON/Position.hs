{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.GeoJSON.Position
-- Copyright   :  (C) 2016 Markus Barenhoff
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Markus Barenhoff <mbarenh@alios.org>
-- Stability   :  provisional
-- Portability :  FlexibleInstances, FunctionalDependencies, TemplateHaskell
--
----------------------------------------------------------------------------

module Data.GeoJSON.Position
 ( Position, PositionStructure, _Position, toPosition3
 , PositionVector, PositionVectorStructure, _PositionVector
 , PositionVector2, PositionVector2Structure, _PositionVector2
 , HasBoundingBox(..), BoundingBox, _BoundingBox
 ) where

import           Control.Applicative
import           Control.Lens.Fold
import           Control.Lens.Getter
import           Control.Lens.Prism
import           Control.Lens.Review
import           Control.Lens.TH
import           Control.Lens.Traversal
import           Data.Aeson
import           Data.GeoJSON.Classes
import           Data.GeoJSON.Intern
import           Data.Maybe             (fromMaybe)
import           Data.Vector            (Vector)

data Position a
  = Position2 (a, a)
  | Position3 (a, a, a)
  deriving (Eq, Show)

instance Functor Position where
  fmap f (Position2 (x,y)) = Position2 (f x, f y)
  fmap f (Position3 (x,y,z)) = Position3 (f x, f y, f z)

newtype BoundingBox a =
  BoundingBox (Position a, Position a)
  deriving (Eq, Show)
makePrisms ''BoundingBox

instance Functor BoundingBox where
  fmap f (BoundingBox (a,b)) = BoundingBox (fmap f a, fmap f b)


type PositionStructure fc a = fc a
type PositionVectorStructure fv fc a = fv (PositionStructure fc a)
type PositionVector2Structure fv fc a = fv (fv (PositionStructure fc a))

type PositionVector a = Vector (Position a)
type PositionVector2 a = Vector (Vector (Position a))


instance BaseType a => ToJSON (Position a) where
  toJSON (Position2 a) = toJSON a
  toJSON (Position3 a) = toJSON a

instance BaseType a => FromJSON (Position a) where
  parseJSON (Array a) =
    sequence (fmap parseJSON a) >>= parserMaybe . preview _Position
  parseJSON _ = empty

_Position :: PositionStructureClass fc a =>
  Prism' (PositionStructure fc a) (Position a)
_Position = prism' fromPosition toPosition

toPosition3 :: Num a => Position a -> Position a
toPosition3 (Position2 (x,y)) = Position3 (x,y, 0)
toPosition3 p@(Position3 _)   = p


toPosition :: (PositionStructureClass fc a) =>
  PositionStructure fc a -> Maybe (Position a)
toPosition = toPosition' (const True) (const True) (const True)

toPositionLatLon :: (PositionStructureClass fc a) =>
  PositionStructure fc a -> Maybe (Position a)
toPositionLatLon = toPosition' isLat isLon isH
  where isLat l = l >= -90 && l <= 90
        isLon l = l >= -180 && l <= 180
        isH = const True

toPosition' :: (PositionStructureClass fc a) =>
  (a -> Bool) -> (a -> Bool) -> (a -> Bool) ->
  PositionStructure fc a -> Maybe (Position a)
toPosition' plat plon ph a = case a ^.. taking 3 traverse of
  [lat,lon,h] -> if plat lat && plon lon && ph h
                 then pure $ Position3 (lat, lon, h)
                 else empty
  [lat,lon]   -> if plat lat && plon lon
                 then pure $ Position2 (lat, lon)
                 else empty
  _           -> empty

fromPosition :: (PositionStructureClass fc a) =>
  Position a -> PositionStructure fc a
fromPosition a = foldMap pure $ case a of
  (Position2 (lat, lon))    -> [lat, lon]
  (Position3 (lat, lon, h)) -> [lat, lon, h]

fromPositionVector :: (VectorStructureClass fv fc a) =>
   PositionVector a -> PositionVectorStructure fv fc a
fromPositionVector = foldMap pure . fmap fromPosition

fromPositionVector2 ::  (VectorStructureClass fv fc a) =>
   PositionVector2 a -> PositionVector2Structure fv fc a
fromPositionVector2 = foldMap pure . fmap fromPositionVector

toPositionVector :: VectorStructureClass fv fc a  =>
  PositionVectorStructure fv fc a  -> Maybe (PositionVector a)
toPositionVector = fmap (foldMap pure) . traverse (preview _Position)

toPositionVector2 :: VectorStructureClass fv fc a =>
  PositionVector2Structure fv fc a  -> Maybe (PositionVector2 a)
toPositionVector2 =  fmap (foldMap pure) . traverse toPositionVector


_PositionLatLon :: PositionStructureClass fc a =>
  Prism' (PositionStructure fc a) (Position a)
_PositionLatLon = prism' fromPosition toPositionLatLon

_PositionVector :: VectorStructureClass fv fc a  =>
  Prism' (PositionVectorStructure fv fc a) (PositionVector a)
_PositionVector = prism' fromPositionVector toPositionVector

_PositionVector2 :: VectorStructureClass fv fc a  =>
  Prism' (PositionVector2Structure fv fc a) (PositionVector2 a)
_PositionVector2 = prism' fromPositionVector2 toPositionVector2


instance BaseType a => HasBoundingBox (Position a) a where
  maxLat (Position2 (x,_))   = x
  maxLat (Position3 (x,_,_)) = x
  maxLon (Position2 (_,y))   = y
  maxLon (Position3 (_,y,_)) = y
  maxHeight (Position2 _)        = 0
  maxHeight (Position3 (_,_, z)) = z
  minLat = maxLat
  minLon = maxLon
  minHeight = maxHeight


takeBoundingBoxArray :: HasBoundingBox t a => Int -> t -> a
takeBoundingBoxArray n a = (boundingBoxArray . boundingBox $ a) !! n

boundingBoxArray :: BaseType a => BoundingBox a -> [a]
boundingBoxArray (BoundingBox (a,b)) =
  let minc = review _Position $ toPosition3 a
      maxc = review _Position $ toPosition3 b
  in minc ++ maxc


class BaseType a => HasBoundingBox t a | t -> a where
  minLat :: t -> a
  minLat = takeBoundingBoxArray 0
  minLon :: t -> a
  minLon = takeBoundingBoxArray 1
  minHeight :: t -> a
  minHeight = takeBoundingBoxArray 2
  maxLat :: t -> a
  maxLat = takeBoundingBoxArray 3
  maxLon :: t -> a
  maxLon = takeBoundingBoxArray 4
  maxHeight :: t -> a
  maxHeight = takeBoundingBoxArray 5
  boundingBox ::  t -> BoundingBox a
  boundingBox a =
    let (minH, maxH) = (minHeight a, maxHeight a)
        minLatLon = [minLat a, minLon a]
        maxLatLon = [maxLat a, maxLon a]
        (minH', maxH') = if minH == 0 && maxH == 0
          then (minLatLon, maxLatLon)
          else (minLatLon ++ [minH], maxLatLon ++ [maxH])
        cmin = preview _Position minH'
        cmax = preview _Position maxH'
    in maybe (error "boundingBox: invalid state.") BoundingBox $
       (,) <$> cmin <*> cmax

instance (BaseType a) => HasBoundingBox (BoundingBox a) a where
  boundingBox = id

instance BaseType a => Monoid (BoundingBox a) where
  mempty = BoundingBox ( Position2 (0,0), Position2 (0,0) )
  mappend a b =
    let cmin = fromMaybe
          (error "mappend BoundingBox: unexpected error") $
          preview _Position
          [ min (minLat a) (minLat b)
          , min (minLon a) (minLon b)
          , min (minHeight a) (minHeight b)
          ]
        cmax = fromMaybe
          (error "mappend BoundingBox: unexpected error") $
          preview _Position
          [ max (maxLat a) (maxLat b)
          , max (maxLon a) (maxLon b)
          , max (maxHeight a) (maxHeight b)
          ]
    in BoundingBox (cmin, cmax)


instance BaseType a => ToJSON (BoundingBox a) where
  toJSON = toJSON . fromBB
    where fromBB a =
            let (x, y) = view _BoundingBox a
            in review _Position x ++ review _Position y
