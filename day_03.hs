import Data.Char ( isDigit )

type Engine = [(Part, Position)]

data Part = Symbol Char | Number [Char]
  deriving (Show)

type Position = (Int, Int)

main :: IO ()
main = do
  engine <- parse <$> readFile "day_03_input.txt"
  print $ sumOfPartNumbers engine

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
sumOfPartNumbers engine = sum [valueOf part | part <- engine]
  where
    valueOf (Number num, pos) = if isPartNumber pos then read num else 0
    valueOf _                 = 0 
    isPartNumber pos = True