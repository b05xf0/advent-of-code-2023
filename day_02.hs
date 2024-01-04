import Data.List ( sortOn )

main :: IO ()
main = do
  doc <- readFile "day_02_input.txt"
  let games = parseGame <$> lines doc
  print $ sum (fst <$> filter possible games)
  print $ sum (powerOfSetNeeded <$> games)

type Game = (Int, [Set])

type Set = [Cube]

type Cube = (String, Int)

config :: Set
config = 
  [("red"  , 12)
  ,("green", 13)
  ,("blue" , 14)]

possible :: Game -> Bool
possible game = all (\(color, count) -> count <= stock color config) $ allSets game

powerOfSetNeeded :: Game -> Int
powerOfSetNeeded game = product $ (\(color, _) -> stock color orderedSets) <$> config
  where orderedSets = reverse . (sortOn snd) $ allSets game 

allSets :: Game -> [Cube]
allSets game = concat $ snd game

stock :: String -> [Cube] -> Int
stock color set = case lookup color set of
  Nothing    -> 0
  Just count -> count

parseGame :: String -> Game
parseGame src = (read gameIdSrc, parseSet <$> splitOn ';' setsSrc)
  where
    (_:_:_:_:_:gameIdSrc, _:setsSrc) = span (/=':') src

parseSet :: String -> Set
parseSet src = parseCube <$> splitOn ',' src

parseCube :: String -> Cube
parseCube src = (color, read countSrc)
  where [countSrc, color] = words src
      
splitOn :: Char -> String -> [String]
splitOn delimiter s = case dropWhile (== delimiter) s of
  "" -> []
  s' -> w : splitOn delimiter s''
    where (w, s'') = break (== delimiter) s'