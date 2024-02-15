import Data.Char ( isDigit )

type Engine = [(Part, Position)]

data Part = Symbol Char | Number [Char]

type Position = (Int, Int)

main :: IO ()
main = do
  engine <- parse <$> readFile "day_03_input.txt"
  print $ sumOfPartNumbers engine
  print $ sumOfGearRatios engine

parse :: String -> Engine
parse raw = parse' raw (0, 0) []
  where
    parse' []         _          processed = processed
    parse' raw@(c:cs) pos@(x, y) processed
      | c == '.'                           = parse' cs (x + 1, y) processed
      | c == '\n'                          = parse' cs (0, y + 1) processed
      | isDigit c                          = let (num, rest) = span isDigit raw
                                             in parse' rest (x + length num, y) $ (Number num, pos):processed
      | otherwise                          = parse' cs (x + 1, y) $ (Symbol c, pos):processed

sumOfPartNumbers :: Engine -> Int
sumOfPartNumbers engine = sum $ partNumber <$> engine
  where
    partNumber number@(Number n, _) = if any (areAdjacent number) engine then read n else 0
    partNumber _                    = 0

sumOfGearRatios :: Engine -> Int
sumOfGearRatios engine = sum $ gearRatio <$> engine
  where
    gearRatio symbol@(Symbol '*', _) = case filter (areAdjacent symbol) engine of
      [(Number n1, _), (Number n2, _)] -> product $ read <$> [n1, n2]
      _                                -> 0
    gearRatio _                      = 0

areAdjacent :: (Part, Position) -> (Part, Position) -> Bool
areAdjacent (Number n, (nX, nY)) (Symbol s, (sX, sY)) = sY `elem` [nY - 1..nY + 1] &&
                                                        sX `elem` [nX - 1..nX + length n]
areAdjacent s@(Symbol _, _)      n@(Number _, _)      = areAdjacent n s
areAdjacent _                    _                    = False