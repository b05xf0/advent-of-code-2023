import Data.List ( intersect )

type Card = ([Int], [Int])

main :: IO ()
main = do
  cards <- parse <$> readFile "day_04_input.txt"
  print $ sum $ calcPoints <$> cards

parse :: String -> [Card]
parse raw = mkCard . extractNumbers <$> lines raw
  where
    extractNumbers line = break (== '|') $ dropWhile (/= ':') line
    mkCard (':':ns1, '|':ns2) = (read <$> words ns1, read <$> words ns2)

calcPoints :: Card -> Int
calcPoints (winningNumbers, myNumbers) = if matches > 0 then 2^(matches - 1) else 0
  where
    matches = length $ intersect winningNumbers myNumbers
