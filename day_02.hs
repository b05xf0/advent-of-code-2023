import Data.Char ( toUpper )

main :: IO ()
main = do
  doc <- readFile "day_02_input.txt"
  let games = parseGame <$> lines doc
  print $ sum (fst <$> filter possible games)

stockConfig :: [Cube]
stockConfig = 
  [("red"  , 12)
  ,("green", 13)
  ,("blue" , 14)]

games :: String -> [Game]
games doc = parseGame <$> lines doc

possible :: Game -> Bool
possible game = all (\(color, count) -> count <= stock color) sets
  where sets = concat $ snd game

stock :: String -> Int
stock color = case lookup color stockConfig of
  Nothing    -> 0
  Just count -> count

type Game = (Int, [Set])

type Set = [Cube]

type Cube = (String, Int)

parseGame :: String -> Game
parseGame src = (gameId, sets)
  where
    (heading, (':':setsSrc)) = span (/=':') src
    [_, gameId] = read <$> words heading
    sets = parseSet <$> splitOn ';' setsSrc

parseSet :: String -> [Cube]
parseSet src = parseCube <$> splitOn ',' src

parseCube :: String -> Cube
parseCube src = (cubeColor, read count)
  where [count, cubeColor] = words src
      
splitOn :: Char -> String -> [String]
splitOn delimiter s = case dropWhile (== delimiter) s of
  "" -> []
  s' -> w : splitOn delimiter s''
    where (w, s'') = break (== delimiter) s'
