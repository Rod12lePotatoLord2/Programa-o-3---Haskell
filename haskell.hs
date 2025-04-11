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
raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = (raizMaior, raizMenor)
  where
    delta = b*b - 4*a*c
    raiz1 = (-b + sqrt' delta) / (2*a)
    raiz2 = (-b - sqrt' delta) / (2*a)
    raizMaior = max raiz1 raiz2
    raizMenor = min raiz1 raiz2

sqrt' :: Float -> Float
sqrt' x = x ** 0.5

x_maior :: Float -> Float -> Float -> Float
x_maior a b c = fst (raizes a b c)

x_menor :: Float -> Float -> Float -> Float
x_menor a b c = snd (raizes a b c)

main :: IO ()
main = do
    let a = 1
    let b = -3
    let c = 2
    print ("Maior raiz: " ++ show (x_maior a b c))
    print ("Menor raiz: " ++ show (x_menor a b c))

-- Questão 7
somaIncluindo :: Int -> Int -> Int
somaIncluindo n1 n2 = sum[n1..n2]

somaExcluindo :: Int -> Int -> Int
somaExcluindo n1 n2 = sum [n1+1..n2-1]

-- Questão 8
multiplosDe :: Int -> Int -> Int -> [Int]
multiplosDe n1 n2 n3 = [x | x <- [n1..n2], x `mod` n3 == 0]

-- Questão 9
multiplicacao :: Int -> Int -> Int
multiplicacao a b
    | b == 0    = 0
    | b > 0     = sum (replicate b a)
    | otherwise = -sum (replicate (-b) a)

-- Questão 10
mod2 :: Int -> Int -> Int
mod2 a b
    | a < 0     = mod2Aux (-a) b
    | otherwise = mod2Aux a b

mod2Aux :: Int -> Int -> Int
mod2Aux a b
    | a < b     = a
    | otherwise = mod2Aux (a - b) b

-- Questão 11
sequenciaRaizAninhada :: Int -> Double
sequenciaRaizAninhada 1 = sqrt 6
sequenciaRaizAninhada n = sqrt (6 + sequenciaRaizAninhada (n - 1))

-- Questão 12
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

coefBinomial :: Int -> Int -> Int
coefBinomial m n
    | m >= n    = fatorial m `div` (fatorial n * fatorial (m - n))
    | otherwise = error "m deve ser maior ou igual a n"

-- Questão 14
dic_10 :: [(Int, String)]
dic_10 = [(0, "zero"), (1, "um"), (2, "dois"), (3, "três"), (4, "quatro"),
          (5, "cinco"), (6, "seis"), (7, "sete"), (8, "oito"), (9, "nove")]

converterParaPalavras :: [Int] -> [String]
converterParaPalavras = map (\x -> lookup' x dic_10)

lookup' :: Int -> [(Int, String)] -> String
lookup' n dic = case lookup n dic of
                  Just palavra -> palavra
                  Nothing -> error "Dígito fora do intervalo de 0 a 9"

-- Questão 15
del_posicao_n :: [Int] -> Int -> [Int]
del_posicao_n [] _ = []
del_posicao_n (x:xs) 0 = xs
del_posicao_n (x:xs) n = x : del_posicao_n xs (n - 1)

-- Questão 16
inserir_posicao_x :: [Int] -> Int -> Int -> [Int]
inserir_posicao_x [] _ y = [y]
inserir_posicao_x (x:xs) 0 y = y : (x:xs)
inserir_posicao_x (x:xs) n y = x : inserir_posicao_x xs (n - 1) y

-- Questão 17
valor_n_esima_posicao :: [Int] -> Int -> Int
valor_n_esima_posicao [] _ = error "Posição fora do alcance da lista"
valor_n_esima_posicao (x:xs) 0 = x
valor_n_esima_posicao (x:xs) n = valor_n_esima_posicao xs (n - 1)

-- Questão 18
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- Questão 19
interseccao :: [Int] -> [Int] -> [Int]
interseccao [] _ = []
interseccao (x:xs) ys
    | x `elem` ys = x : interseccao xs ys
    | otherwise   = interseccao xs ys