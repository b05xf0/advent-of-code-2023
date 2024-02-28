module Main where

main :: IO ()
main = do
  (seeds, mappings) <- parse <$> readFile "day_05_input.txt"
  let locations     = convert mappings <$> seeds
  putStrLn $ "d05p1: " <> show (minimum $ (\(Location id) -> id) <$> locations)

type Start  = Int
type Length = Int
type Key    = String

type Mapping = (Key, [(Start, Start, Length)])

data Category = Seed Int
              | Soil Int
              | Fertilizer Int
              | Water Int
              | Light Int
              | Temperature Int
              | Humidity Int
              | Location Int
  deriving ( Show )

parse :: String -> ([Category], [Mapping])
parse raw = parse' (words <$> lines raw) ([], [])
  where
    parse' src parsed = case (src, parsed) of
      ([]:rest, parsed)
        -> parse' rest parsed
      (("seeds:":ss):rest, (seeds, mappings))
        -> parse' rest (Seed . read <$> ss, mappings)
      ((key:"map:":_):rest, (seeds, mappings))
        -> parse' (dropWhile (/= []) rest) (seeds, (key, parseMappings (map (read <$>) (takeWhile (/= []) rest))) : mappings)
      (_, parsed) -> parsed

parseMappings :: [[Int]] -> [(Start, Start, Length)]
parseMappings ms = case ms of
  [startDest, startSrc, length]:rest -> (startDest, startSrc, length) : parseMappings rest
  _                                  -> []

convert :: [Mapping] -> Category -> Category
convert mappings sth = case sth of
  Seed id        -> convert mappings (Soil (id `searchIn` "seed-to-soil"))
  Soil id        -> convert mappings (Fertilizer (id `searchIn` "soil-to-fertilizer"))
  Fertilizer id  -> convert mappings (Water (id `searchIn` "fertilizer-to-water"))
  Water id       -> convert mappings (Light (id `searchIn` "water-to-light"))
  Light id       -> convert mappings (Temperature (id `searchIn` "light-to-temperature"))
  Temperature id -> convert mappings (Humidity (id `searchIn` "temperature-to-humidity"))
  Humidity id    -> Location (id `searchIn` "humidity-to-location") 
  where
    searchIn id mappingKey = case [startDest + id - startSrc |
                                     (key, mapping) <- mappings,
                                     key == mappingKey,
                                     (startDest, startSrc, length) <- mapping,
                                     id >= startSrc && id < startSrc + length] of
      [newId] -> newId
      _       -> id