{-# LANGUAGE DeriveGeneric #-}

module ConfigVals
( decodeYAML
, PHPClass
, className
, implements
, properties
, PHPProperty
, name
, typeHint
, getterName
, serializationOptions
, extends
, locked
, serializable
) where

import GHC.Generics
import Data.Aeson
import Data.Yaml
import Control.Applicative
import qualified Data.ByteString.Char8 as BS

decodeYAML :: FilePath -> IO (Either String [PHPClass])
decodeYAML yamlFile = decodeEither <$> BS.readFile yamlFile

data PHPClass =
  PHPClass {
          className  :: String
          , implements :: Maybe [String]
          , extends :: Maybe String
          , properties :: [PHPProperty]
          , locked :: Maybe Bool
          , serializable :: Maybe Bool
          } deriving (Generic, Show)

data PHPProperty =
  PHPProperty {
          name  :: String
          , typeHint :: Maybe String
          , getterName :: Maybe String
          , serializationOptions :: Maybe String
          } deriving (Generic, Show)

instance FromJSON PHPClass
instance FromJSON PHPProperty

