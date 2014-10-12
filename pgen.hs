import Output
import ConfigVals
import System.Console.ArgParser

main :: IO ()
main = withParseResult cliConfigParser generate

generate (CliConfig  outputDir) = do
 d <- decodeYAML "./classes.yml"
 case d of
   Left err -> putStrLn err
   Right ps -> mapM_ (outputTemplate "./templates/simple.ha" outputDir) ps

data CliConfig = CliConfig String
  deriving (Show)

cliConfigParser :: ParserSpec CliConfig
cliConfigParser = CliConfig
  `parsedBy` optPos "./src" "outputDir"