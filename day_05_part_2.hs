module Main where

import Data.List ( sortOn )
import Data.Maybe ( fromMaybe )

type Start  = Int
type Length = Int
type Key    = String
type Range  = (Start, Length)

type Mapping = (Key, [(Start, Start, Length)])

data CategoryRange = Seed Range
                   | Soil Range
                   | Fertilizer Range
                   | Water Range
                   | Light Range
                   | Temperature Range
                   | Humidity Range
                   | Location Range
  deriving ( Show )

main :: IO ()
main = do
  (seeds, mappings) <- parse <$> readFile "day_05_input.txt"
  let locations = concatMap (convert mappings) seeds
  putStrLn $ "d05p2: " <> show (minimum $ (\(Location (start, _) ) -> start) <$> locations)

parse :: String -> ([CategoryRange], [Mapping])
parse raw = parse' (words <$> lines raw) ([], [])
  where
    parse' src parsed = case (src, parsed) of
      ([]:rest, parsed)
        -> parse' rest parsed
      (("seeds:":ss):rest, (seeds, mappings))
        -> parse' rest (parseSeeds (read <$> ss), mappings)
      ((key:"map:":_):rest, (seeds, mappings))
        -> parse' (dropWhile (/= []) rest)
                  (seeds, (key, sortOn (\(_, start, _) -> start) $ parseMappings (map (read <$>) (takeWhile (/= []) rest))) : mappings)
      (_, parsed) -> parsed

parseSeeds :: [Int] -> [CategoryRange]
parseSeeds ss = case ss of
  start:length:rest -> Seed (start, length) : parseSeeds rest
  _                 -> []

parseMappings :: [[Int]] -> [(Start, Start, Length)]
parseMappings ms = case ms of
  [startDest, startSrc, length]:rest -> (startDest, startSrc, length) : parseMappings rest
  _                                  -> []

convert :: [Mapping] -> CategoryRange -> [CategoryRange]
convert mappings sth = case sth of
  Seed range        -> concatMap (convert mappings <$> Soil) (convertRange range (fromMaybe [] (lookup "seed-to-soil" mappings)))
  Soil range        -> concatMap (convert mappings <$> Fertilizer) (convertRange range (fromMaybe [] (lookup "soil-to-fertilizer" mappings)))
  Fertilizer range  -> concatMap (convert mappings <$> Water) (convertRange range (fromMaybe [] (lookup "fertilizer-to-water" mappings)))
  Water range       -> concatMap (convert mappings <$> Light) (convertRange range (fromMaybe [] (lookup "water-to-light" mappings)))
  Light range       -> concatMap (convert mappings <$> Temperature) (convertRange range (fromMaybe [] (lookup "light-to-temperature" mappings)))
  Temperature range -> concatMap (convert mappings <$> Humidity) (convertRange range (fromMaybe [] (lookup "temperature-to-humidity" mappings)))
  Humidity range    -> Location <$> convertRange range (fromMaybe [] (lookup "humidity-to-location" mappings))
  _ -> []

convertRange :: Range -> [(Start, Start, Length)] -> [Range]
convertRange range mapping = case (range, mapping) of
  (range, []) -> [range]
  (range@(start, length), mapping@((mappingStartDest, mappingStart, mappingLength):ms))
    | end < mappingStart
      -> [(start, length)]
    | mappingEnd < start
      -> convertRange range ms
    | start >= mappingStart && end <= mappingEnd
      -> [(start + shift, length)]
    | start >= mappingStart && end > mappingEnd
      -> (start + shift, mappingEnd - start) : convertRange (mappingEnd, length - (mappingEnd - start)) ms
    | start < mappingStart
      -> (start, mappingStart - start) : convertRange (mappingStart, length - (mappingStart - start)) mapping
    where
      end = start + length - 1
      mappingEnd = mappingStart + mappingLength - 1
      shift = mappingStartDest - mappingStart
