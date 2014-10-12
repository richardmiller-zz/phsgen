import Output
import ConfigVals
import System.Console.ArgParser

main :: IO ()
main = withParseResult cliConfigParser generate

generate (CliConfig outputDir inputFile) = do
 d <- decodeYAML inputFile
 case d of
   Left err -> putStrLn err
   Right ps -> mapM_ (outputTemplate "./templates/simple.ha" outputDir) ps

data CliConfig = CliConfig String String
  deriving (Show)

cliConfigParser :: ParserSpec CliConfig
cliConfigParser = CliConfig
  `parsedBy` optPos "./src" "outputDir"
  `andBy` optPos "./classes.yml" "inputFile"