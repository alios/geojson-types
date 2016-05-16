module Data.GeoJSON
       ( module Data.GeoJSON.Objects,
         module Data.GeoJSON.Features,
         module Data.GeoJSON.Classes,
         readFeatureCollection
       ) where

import Data.GeoJSON.Objects
import Data.GeoJSON.Features
import Data.GeoJSON.Classes
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL

-- | read a GeoJSON 'FeatureCollection' from file.
readFeatureCollection ::
  BaseType t => FilePath -> IO (Either String (FeatureCollection Aeson.Value t))
readFeatureCollection f = Aeson.eitherDecode <$>  BL.readFile f

