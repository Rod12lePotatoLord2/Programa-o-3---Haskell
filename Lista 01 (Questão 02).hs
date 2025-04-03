maioresQueMedia :: (Ord a, Fractional a) => a -> a -> a -> Int
maioresQueMedia a b c = length [x | x <- [a, b, c], x > media]
  where
    media = (a + b + c) / 3