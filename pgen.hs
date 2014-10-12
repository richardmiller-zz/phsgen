import Output
import ConfigVals

main :: IO ()
main = do
 d <- decodeYAML "./classes.yml"
 case d of
   Left err -> putStrLn err
   Right ps -> mapM_ (outputTemplate "./simple.ha") ps
