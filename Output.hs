{-# LANGUAGE DeriveDataTypeable #-}

module Output
( outputTemplate
) where

import Text.Hastache
import Text.Hastache.Context
import Data.Data
import Data.Maybe
import Data.List
import Data.List.Split
import Data.String.Utils
import System.Directory
import Control.Applicative
import qualified Control.Monad as C
import qualified Data.Text.Lazy.IO as TL
import qualified ConfigVals as CV

outputTemplate templateFilename outputRoot vars = do
    createDirectoryIfMissing True (outputDirectory cName outputRoot)
    rendered <- hastacheStr defaultConfig (encodeStr template) (mkGenericContext (toTemplateVars vars))
    fileExists <- doesFileExist fName
    C.unless (fileExists && fromMaybe False (CV.locked vars)) $ TL.writeFile fName rendered
        where
            fName = outputFilename cName outputRoot
            cName = CV.className vars
            outputFilename cName = addOutputRoot $ cName ++ ".php"
            outputDirectory cName = addOutputRoot $ dirPath cName
            dirPath = join "\\" . init . splitNamespace
            addOutputRoot path outputRoot = concat [outputRoot, "/", map replaceSeparators $ path]
            replaceSeparators = (\c -> if c=='\\' then '/'; else c)

data PClass =
  PClass {
          className :: String
          , namespace :: String
          , imports :: String
          , properties :: [PProperty]
          , argumentsList :: String
          , implements :: String
          , extends :: String
          , interfaceImports :: String
          , parentImports :: String
          , serializationMethods :: Bool
          , deserializationArguments :: String
          , serializationArguments :: String
          } deriving (Data, Typeable, Show)

data PProperty =
  PProperty {
          name :: String
          , getterName :: String
          , typeHint :: Maybe String
          , serializationOptions :: Maybe String
          } deriving (Data, Typeable, Show)

toTemplateVars :: CV.PHPClass -> PClass
toTemplateVars fromClass = PClass {
        className = classNameFromFqcn cName
        , namespace = namespaceStr cName
        , properties = tProps
        , imports = importsStr tProps
        , interfaceImports = interfaceImportsStr cImplements
        , parentImports = parentImportsStr cExtends
        , argumentsList = args tProps
        , implements = implementsStr cImplements
        , extends = extendsStr cExtends
        , serializationMethods = sMethods
        , deserializationArguments = deserializationArgs tProps
        , serializationArguments = serializationArgs tProps
    }
    where
        cName = CV.className fromClass
        cImplements = CV.implements fromClass
        cExtends = CV.extends fromClass
        sMethods = fromMaybe False $ CV.serializable fromClass
        tProps = map toTemplateProperty . CV.properties $ fromClass
        toTemplateProperty fromProperty = PProperty {
            name = CV.name fromProperty
            , getterName = fromMaybe (CV.name fromProperty) (CV.getterName fromProperty)
            , typeHint = CV.typeHint fromProperty
            , serializationOptions = CV.serializationOptions fromProperty
        }

args :: [PProperty] -> String
args = intercalate ", " . map phpify
    where
        phpify x = concat [typeHintStr $ typeHint x, "$", name x]
        typeHintStr hint = fromMaybe "" $ C.liftM2 (++) (classNameFromFqcn <$> hint) $ Just " "

importsStr :: [PProperty] -> String
importsStr = formatUseStatements . map (mImportsStr . typeHint)

interfaceImportsStr :: Maybe [String] -> String
interfaceImportsStr interfaces = fromMaybe "" $ formatUseStatements <$> mapUseStatements interfaces
    where mapUseStatements = fmap (C.liftM useStatement)

implementsStr Nothing = ""
implementsStr (Just interfaces) = concat [" implements ", join ", " . map classNameFromFqcn $ interfaces]

parentImportsStr str = mImportsStr str ++ "\n"

namespaceStr = ns . splitNamespace
    where
        ns []     = ""
        ns [x]    = ""
        ns (x:xs) = concat ["\nnamespace ", join "\\" $ init $ x:xs, ";\n"]

extendsStr parent = fromMaybe "" $ ((++) " extends ") <$> classNameFromFqcn <$> parent

formatUseStatements = addLineBreaks . intercalate "\n" . removeLocal . removeNull
    where
        removeNull = filter $ not . null
        removeLocal = filter $ namespaced . splitNamespace
        namespaced [x]    = False
        namespaced (x:xs) = True
        addLineBreaks str
          | null str  = str
          | otherwise = concat ["\n", str, "\n"]

mImportsStr str = fromMaybe "" $ useStatement <$> str

splitNamespace = splitOn "\\"

classNameFromFqcn  = last . splitNamespace

useStatement fqcn = concat ["use ", fqcn, ";"]

deserializationArgs :: [PProperty] -> String
deserializationArgs = intercalate ",\n            " . map phpify
    where
        phpify x = withOptions (name x) (serializationOptions x)
        withOptions propName (Just "optional") = concat ["isset($data['", propName, "']) ? $data['", propName, "'] : null"]
        withOptions propName (Just "time")     = concat ["new \\DateTime($data['", propName, "'])"]
        withOptions propName _                 = concat ["$data['", propName, "']"]

serializationArgs :: [PProperty] -> String
serializationArgs = intercalate ",\n            " . map phpify
    where
        phpify x = withOptions (name x) (serializationOptions x)
        withOptions propName (Just "stringCast")   = concat ["'", propName, "' => (string) $this->", propName]
        withOptions propName (Just "time")         = concat ["'", propName, "' => $this->", propName, "->format(DATE_ISO8601)"]
        withOptions propName (Just "serializable") = concat ["'", propName, "' => $this->", propName, " ? $this->", propName, "->serialize() : null"]
        withOptions propName _                     = concat ["'", propName, "' => $this->", propName]

template = concat ["<?php\n",
            "{{{namespace}}}{{{imports}}}{{{interfaceImports}}}{{{parentImports}}}\n",
            "class {{className}}{{extends}}{{implements}}\n",
            "{\n",
            "{{#properties}}\n",
            "    private ${{name}};\n",
            "{{/properties}}\n",
            "\n",
            "    public function __construct({{argumentsList}})\n",
            "    {\n",
            "{{#properties}}\n",
            "        $this->{{name}} = ${{name}};\n",
            "{{/properties}}\n",
            "    }\n",
            "{{#properties}}\n",
            "\n",
            "    public function {{getterName}}()\n",
            "    {\n",
            "        return $this->{{name}};\n",
            "    }\n",
            "{{/properties}}\n",
            "{{#serializationMethods}}\n",
            "\n",
            "    public function serialize()\n",
            "    {\n",
            "        return [\n",
            "            {{{serializationArguments}}}\n",
            "        ];\n",
            "    }\n",
            "\n",
            "    public static function deserialize(array $data)\n",
            "    {\n",
            "        return new self(\n",
            "            {{{deserializationArguments}}}\n",
            "        );\n",
            "    }\n",
            "{{/serializationMethods}}\n",
            "}"]