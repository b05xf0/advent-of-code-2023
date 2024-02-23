{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Data.List ( intersect )

type Card = ([Int], [Int])

main :: IO ()
main = do
  cards <- parse <$> readFile "day_04_input.txt"
  print $ sum $ calcPoints . snd <$> cards
  print $ sum $ fst <$> process cards
  print $ process cards

parse :: String -> [(Integer, Card)]
parse raw = (1, ) . mkCard . extractNumbers <$> lines raw
  where
    extractNumbers line = break (== '|') $ dropWhile (/= ':') line
    mkCard (':':ns1, '|':ns2) = (read <$> words ns1, read <$> words ns2)

calcPoints :: Card -> Int
calcPoints (winningNumbers, myNumbers) = if matches > 0 then 2^(matches - 1) else 0
  where
    matches = length $ intersect winningNumbers myNumbers

process :: [(Integer, Card)] -> [(Integer, Card)]
process cards = go cards [] []
  where
    go []                 processed _      = processed
    go ((cnt, card):rest) processed (w:ws) = go rest ((cnt + w, card):processed) (redeem (cnt + w, card) `merge` ws)
    go ((cnt, card):rest) processed _     = go rest ((cnt, card):processed) (redeem (cnt, card))
    redeem (cnt, card) = replicate (calcPoints card) cnt
    merge []   m    = m
    merge t    []   = t
    merge (t:ts) (m:ms) = (t + m):merge ts ms
