import Data.Char ( toUpper )

data Cube = Green Int | Red Int | Blue Int deriving (Show, Read, Eq)

type Set = [Cube]

data Game = Game Int [Set] deriving (Show, Read, Eq)

parseGame = span (/=':')

parseCube :: String -> Cube
parseCube src = read $ unwords [toUpper colorHead:colorTail, count]
  where
    [count, colorHead:colorTail] = words src


