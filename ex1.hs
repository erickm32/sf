osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d =
  if (a == b) && (b == c) && (c == d) then True
  else False


quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c
  | (a == b) && (b == c) = 3
  | a == b || a == c = 2
  | b == c = 2
  | otherwise = 0
