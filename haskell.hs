-- Questão 1
contarIguais :: Eq a => a -> a -> a -> Int
contarIguais a b c
  | a == b && b == c = 3
  | a == b || a == c || b == c = 2
  | otherwise = 0

-- Questão 2
maioresQueMedia :: (Ord a, Fractional a) => a -> a -> a -> Int
maioresQueMedia a b c = length [x | x <- [a, b, c], x > media]
  where
    media = (a + b + c) / 3

-- Questão 3
potencia_2 :: Num a => a -> a
potencia_2 x = x * x

-- Questão 4
potencia_4 :: Num a => a -> a
potencia_4 x = potencia_2 (potencia_2 x)

-- Questão 5
ouExclusivo :: Bool -> Bool -> Bool
ouExclusivo x y = (x || y) && not (x && y)

-- Questão 6
x_maior :: (Floating a, Ord a) => a -> a -> a -> Maybe a
x_maior a b c
   delta < 0 = Nothing
   otherwise = Just ((-b + sqrt delta) / (2 * a))
   where delta = b^2 - 4 * a * c
