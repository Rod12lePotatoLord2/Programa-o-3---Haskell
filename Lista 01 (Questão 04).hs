potencia_2 :: Num a => a -> a
potencia_2 x = x * x

potencia_4 :: Num a => a -> a
potencia_4 x = potencia_2 (potencia_2 x)