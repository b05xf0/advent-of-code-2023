import Data.Char ( isDigit )
import Data.List ( isPrefixOf )

fixed :: Bool
fixed = True

main :: IO ()
main = do
  doc <- readFile "day_01_input.txt"
  print $ sumOfCalibrationValues doc

sumOfCalibrationValues :: String -> Int
sumOfCalibrationValues calibrationDoc = sum $ (calibrationValue []) <$> lines calibrationDoc 

calibrationValue :: String -> String -> Int
calibrationValue []           []                   = 0
calibrationValue [x]          []                   = read [x, x]
calibrationValue digits       []                   = read digits
calibrationValue []           line@(x:xs)
  | isDigit x                                      = calibrationValue [x] xs
  | fixed                                          = calibrationValue (digitSpelledOutWithLetters line) xs
  | otherwise                                      = calibrationValue [] xs
calibrationValue digits@(y:_) line@(x:xs)
  | isDigit x                                      = calibrationValue [y, x] xs
  | fixed && digitSpelledOutWithLetters line /= [] = calibrationValue (y:digitSpelledOutWithLetters line) xs
  | otherwise                                      = calibrationValue digits xs

digitSpelledOutWithLetters :: String -> String
digitSpelledOutWithLetters line = take 1 [v | (k, v) <- dict, k `isPrefixOf` line]
  where
    dict =
      [("one"  , '1')
      ,("two"  , '2')
      ,("three", '3')
      ,("four" , '4')
      ,("five" , '5')
      ,("six"  , '6')
      ,("seven", '7')
      ,("eight", '8')
      ,("nine" , '9')
      ,("zero" , '0')]
