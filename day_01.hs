import Data.Char

main :: IO ()
main = do
  doc <- readFile "day_01_input.txt"
  print $ sumOfCalibrationValues doc

sumOfCalibrationValues :: String -> Int
sumOfCalibrationValues calibrationDoc = sum $ read . getFirstAndLast <$> getNumbers 
  where
    getNumbers = filter isDigit <$> lines calibrationDoc
    getFirstAndLast numbers = case numbers of
      []     -> "0"
      (x:[]) -> [x, x]
      (x:xs) -> [x, last xs]
