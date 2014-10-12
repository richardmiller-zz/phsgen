{-# LANGUAGE DeriveDataTypeable #-}

module Output
( outputTemplate
) where

import Text.Hastache
import Text.Hastache.Context
import Data.Data
import Data.List
import Data.List.Split
import Data.String.Utils
import System.Directory
import qualified Data.Text.Lazy.IO as TL
import qualified ConfigVals as CV

outputTemplate templateFilename outputRoot vars = do
    createDirectoryIfMissing True (outputDirectory outputRoot vars)
    rendered <- hastacheFile defaultConfig templateFilename (mkGenericContext (toTemplateVars vars))
    TL.writeFile (outputFilename outputRoot vars) rendered

outputFilename outputRoot fromClass = concat [outputRoot, "/", map replaceSeparators $ CV.className fromClass, ".php"]

outputDirectory outputRoot fromClass = concat [outputRoot, "/", map replaceSeparators $ dirPath (CV.className fromClass)]
    where
        dirPath = join "\\" . init . splitNamespace

replaceSeparators = (\c -> if c=='\\' then '/'; else c)

data PClass =
  PClass {
          className  :: String
          , namespace  :: String
          , imports :: String
          , properties :: [PProperty]
          , argumentsList :: String
          } deriving (Data, Typeable, Show)

data PProperty =
  PProperty {
          name  :: String
          , typeHint  :: Maybe String
          } deriving (Data, Typeable, Show)

toTemplateVars :: CV.PHPClass -> PClass
toTemplateVars fromClass = PClass {
        className = last . splitNamespace $ cName
        , namespace = namespaceStr cName
        , properties = tProps
        , imports = importsStr tProps
        , argumentsList = args tProps
    }
    where
        cName = CV.className fromClass
        tProps = map toTemplateProperty . CV.properties $ fromClass
        toTemplateProperty fromProperty = PProperty {
            name = CV.name fromProperty
            , typeHint = CV.typeHint fromProperty
        }

splitNamespace = splitOn "\\"

args :: [PProperty] -> String
args = intercalate ", " . map phpify
    where
        phpify x = concat [typeHintStr $ typeHint x, "$", name x]
        typeHintStr Nothing     = ""
        typeHintStr (Just hint) = last . splitNamespace $ hint ++ " "

importsStr :: [PProperty] -> String
importsStr = addLineBreaks . intercalate "\n" . removeLocal . removeNull . map importStatement
    where
        addLineBreaks str
          | null str  = str
          | otherwise = concat ["\n", str, "\n"]
        removeNull = filter $ not . null
        removeLocal = filter $ namespaced . splitNamespace
            where
                namespaced [x]    = False
                namespaced (x:xs) = True
        importStatement = importStr . typeHint
            where
                importStr Nothing     = ""
                importStr (Just hint) = concat ["use ", hint, ";"]

namespaceStr = ns . splitNamespace
    where
        ns []     = ""
        ns [x]    = ""
        ns (x:xs) = concat ["\nnamespace ", join "\\" $ init $ x:xs, ";\n"]
