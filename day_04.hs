import Data.List ( intersect )

type Card = ([Int], [Int])

main :: IO ()
main = do
  cards <- parse <$> readFile "day_04_input.txt"
  putStrLn $ "d04p1: " <> show (sum $ calcPoints . snd <$> cards)
  putStrLn $ "d04p2: " <> show (sum $ fst <$> process cards)

parse :: String -> [(Int, Card)]
parse raw = (1, ) . mkCard . split <$> lines raw
  where
    split = break (== '|') . dropWhile (/= ':')
    mkCard (':':ns1, '|':ns2) = (parseNumbers ns1, parseNumbers ns2)

parseNumbers :: String -> [Int]
parseNumbers ns = read <$> words ns

calcPoints :: Card -> Int
calcPoints card = if matches card > 0 then 2^(matches card - 1) else 0

matches :: Card -> Int
matches (winningNumbers, myNumbers) = length $ winningNumbers `intersect` myNumbers

process :: [(Int, Card)] -> [(Int, Card)]
process cards = process' cards []
  where process' cards copies = case (cards, copies) of
          ([],               _   ) -> []
          ((cnt, card):rest, []  ) -> (cnt,     card) : process' rest (redeem cnt card)
          ((cnt, card):rest, c:cs) -> (cnt + c, card) : process' rest (redeem (cnt + c) card `add` cs)

add :: [Int] -> [Int] -> [Int]
add new old = case (new, old) of
  ([],   os  ) -> os
  (ns,   []  ) -> ns
  (n:ns, o:os) -> (n + o) : add ns os

redeem :: Int -> Card -> [Int]
redeem cnt card = replicate (matches card) cnt
