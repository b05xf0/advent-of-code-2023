module Main where

main :: IO ()
main = do
  (seeds, mappings) <- parse <$> readFile "day_05_input.txt"
  print seeds
  print mappings

data Category = Seed Int
              | Soil Int
              | Fertilizer Int
              | Water Int
              | Light Int
              | Temperature Int
              | Humidity Int
              | Location Int
  deriving ( Show )

type Mapping = (String, [(Int, Int, Int)])

parse :: String -> ([Category], [Mapping])
parse raw = parse' (words <$> lines raw) ([], []) 
  where 
    parse' src parsed = case (src, parsed) of
      ([], parsed) -> parsed
      ([]:rest, parsed) -> parse' rest parsed
      (("seeds:":ss):rest, (seeds, mappings)) -> parse' rest (Seed . read <$> ss , mappings)
      ((key:"map:":_):rest, (seeds, mappings)) -> parse' (dropWhile (/= []) rest) (seeds, parseMapping rest:mappings)
      (_, parsed) -> parsed

parseMapping = undefined