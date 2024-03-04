module Main where

import Data.Maybe ( fromMaybe )

type Time     = Int
type Distance = Int
type Race     = (Time, Distance)

main :: IO ()
main = do
  races <- parse <$> readFile "day_06_input.txt"
  putStrLn $ "d06p1: " <> show (product $ noWaysToBeatRecord <$> maybe [] fst races)
  putStrLn $ "d06p2: " <> show (noWaysToBeatRecord $ maybe (0, 0) snd races)

parse :: String -> Maybe ([Race], Race)
parse races = case words <$> lines races of
  ["Time:":ts, "Distance:":ds] -> Just (zipWith (\ t d -> (read t, read d)) ts ds, (read $ concat ts, read $ concat ds))
  _                            -> Nothing

noWaysToBeatRecord :: Race -> Int
--noWaysToBeatRecord (time, record) = sum [1 | t <- [1..time], t * (time - t) > record]
noWaysToBeatRecord (time, record) = min (floor r2) time - max (ceiling r1) 1 + 1
  where (r1, r2) = fromMaybe (0, 0) (rootsOfQuadraticEquation (-1) (fromIntegral time) (fromIntegral (-record)))

rootsOfQuadraticEquation :: (Floating b, Ord b) => b -> b -> b -> Maybe (b, b)
rootsOfQuadraticEquation a b c
  | d < 0     = Nothing
  | otherwise = Just ((-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a))
  where d = b^2 - 4 * a * c