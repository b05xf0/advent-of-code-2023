main :: IO ()
main = do
  engine <- readFile "day_03_input.txt"
  let (width, height) = dim engine
  print (width, height)

dim :: String -> (Int, Int)
dim engine = case lines engine of
  []       -> (0,0)
  ls@(l:_) -> (length l, length ls)