{-# LANGUAGE DeriveGeneric #-}

module ConfigVals
( decodeYAML
, PHPClass
, className
, properties
, PHPProperty
, name
, typeHint
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
          , properties :: [PHPProperty]
          } deriving (Generic, Show)

data PHPProperty =
  PHPProperty {
          name  :: String
          , typeHint  :: Maybe String
          } deriving (Generic, Show)

instance FromJSON PHPClass
instance FromJSON PHPProperty

