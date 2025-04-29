-- Questão 1: Fornecidos três valores a, b e c, escreva uma função que retorne quantos dos três são iguais. A resposta pode ser 3 (todos iguais), 2 (dois iguais e o terceiro diferente) ou 0 (todos diferentes)
contarIguais :: Eq a => a -> a -> a -> Int
contarIguais a b c
  | a == b && b == c = 3
  | a == b || a == c || b == c = 2
  | otherwise = 0

-- Questão 2: Fornecidos três valores a, b e c, elaborar uma função que retorne quantos desses três valores são maiores que a média entre eles
maioresQueMedia :: (Ord a, Fractional a) => a -> a -> a -> Int
maioresQueMedia a b c = length [x | x <- [a, b, c], x > media]
  where
    media = (a + b + c) / 3

-- Questão 3: Escreva uma função potencia_2 que retorne o quadrado de um número (x2)
potencia_2 :: Num a => a -> a
potencia_2 x = x * x

-- Questão 4: Reutilizando a função potencia_2, construir uma função potencia_4 que retorne o seu argumento elevado à quarta potência
potencia_4 :: Num a => a -> a
potencia_4 x = potencia_2 (potencia_2 x)

-- Questão 5: Implemente em Haskell a função do ou-exclusivo
ouExclusivo :: Bool -> Bool -> Bool
ouExclusivo x y = (x || y) && not (x && y)

-- Questão 6: Escrever duas funções, x_maior que retorne o maior e x_menor que retorne o menor valor real, das raízes de uma equação do segundo grau
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

-- Questão 7: Criar funções que calculam a soma dos números entre n1 e n2, incluindo e excluindo os limites
somaIncluindo :: Int -> Int -> Int
somaIncluindo n1 n2 = sum[n1..n2]

somaExcluindo :: Int -> Int -> Int
somaExcluindo n1 n2 = sum [n1+1..n2-1]

-- Questão 8: Dados dois números n1 e n2, encontrar os múltiplos de n3 que se encontram nesse intervalo (inclusivo)
multiplosDe :: Int -> Int -> Int -> [Int]
multiplosDe n1 n2 n3 = [x | x <- [n1..n2], x `mod` n3 == 0]

-- Questão 9: Utilizando a função sum, faça uma função que calcule a multiplicação entre dois números quaisquer, considerando números positivos e negativos
multiplicacao :: Int -> Int -> Int
multiplicacao a b
    | b == 0    = 0
    | b > 0     = sum (replicate b a)
    | otherwise = -sum (replicate (-b) a)

-- Questão 10: Implemente a função mod2, que retorna o resto de uma divisão de inteiros. OBS: não é permitido usar a função mod nem a função rem da biblioteca
mod2 :: Int -> Int -> Int
mod2 a b
    | a < 0     = mod2Aux (-a) b
    | otherwise = mod2Aux a b

mod2Aux :: Int -> Int -> Int
mod2Aux a b
    | a < b     = a
    | otherwise = mod2Aux (a - b) b

-- Questão 11 - Seja a sequência (Ele coloca sequência envolvendo 6 e raízes de 6) Escreva a função que calcula essa sequência
sequenciaRaizAninhada :: Int -> Double
sequenciaRaizAninhada 1 = sqrt 6
sequenciaRaizAninhada n = sqrt (6 + sequenciaRaizAninhada (n - 1))

-- Questão 12: Implementar a fórmula que indica de quantas maneiras é possível escolher n objetos de uma coleção original de m objetos, onde m >= n
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

coefBinomial :: Int -> Int -> Int
coefBinomial m n
    | m >= n    = fatorial m `div` (fatorial n * fatorial (m - n))
    | otherwise = error "m deve ser maior ou igual a n"

-- Questão 13: Defina uma função que, dada uma lista numérica, retorne uma tupla, que contenha o maior da lista bem como sua posição relativa
maiorValorComPosicao :: (Ord a) => [a] -> (a, Int)
maiorValorComPosicao [] = error "Lista Vazia"
maiorValorComPosicao (x:xs) = aux xs x 0 0
  where
    aux [] maior posAtual posMaior = (maior, posMaior)
    aux (y:ys) maior posAtual posMaior
      | y > maior = aux ys y (posAtual + 1) (posAtual + 1)
      | otherwise = aux  ys maior (posAtual + 1) posMaior

-- Questão 14: Defina uma função que converta uma lista de dígitos (unitários, 0 a 9) em uma outra lista, que é a sua tradução em String (Ele colocou o dicionário na lista)
dic_10 :: [(Int, String)]
dic_10 = [(0, "zero"), (1, "um"), (2, "dois"), (3, "três"), (4, "quatro"),
          (5, "cinco"), (6, "seis"), (7, "sete"), (8, "oito"), (9, "nove")]

converterParaPalavras :: [Int] -> [String]
converterParaPalavras = map (\x -> lookup' x dic_10)

lookup' :: Int -> [(Int, String)] -> String
lookup' n dic = case lookup n dic of
                  Just palavra -> palavra
                  Nothing -> error "Dígito fora do intervalo de 0 a 9"

-- Questão 15: Construa uma função del_posicao_n :: [Int] -> Int -> [Int] em que dada uma lista de inteiros e a posição de um elemento qualquer, retorne uma nova lista sem aquele elemento da n-ésima posição
del_posicao_n :: [Int] -> Int -> [Int]
del_posicao_n [] _ = []
del_posicao_n (x:xs) 0 = xs
del_posicao_n (x:xs) n = x : del_posicao_n xs (n - 1)

-- Questão 16: Construa uma função inserir_posicao_x :: [Int] -> Int -> Int -> [Int] em que, dada uma lista de inteiros, uma posição e um elemento a ser inserido, retorne uma nova lista com aquele elemento na n-ésima posição
inserir_posicao_x :: [Int] -> Int -> Int -> [Int]
inserir_posicao_x [] _ y = [y]
inserir_posicao_x (x:xs) 0 y = y : (x:xs)
inserir_posicao_x (x:xs) n y = x : inserir_posicao_x xs (n - 1) y

-- Questão 17: Defina uma função que retorne o valor da n-ésima posição de uma lista
valor_n_esima_posicao :: [Int] -> Int -> Int
valor_n_esima_posicao [] _ = error "Posição fora do alcance da lista"
valor_n_esima_posicao (x:xs) 0 = x
valor_n_esima_posicao (x:xs) n = valor_n_esima_posicao xs (n - 1)

-- Questão 18: Dadas duas listas ordenadas como entrada, faça uma função merge, que une as duas listas
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- Questão 19: Implemente uma função que receba duas listas de inteiros sem repetição, e retorne uma terceira lista que contenha somente números que estejam nas duas listas dadas
interseccao :: [Int] -> [Int] -> [Int]
interseccao [] _ = []
interseccao (x:xs) ys
    | x `elem` ys = x : interseccao xs ys
    | otherwise   = interseccao xs ys

-- Questão 20: Crie uma função que faça uma codificação sobre uma sequência de caracteres iguais, substitua a sequência por !na, onde n é o número de vezes que o caractere a é repetido. Observe que só devem ser comprimidas sequências de tamanhos maiores que 3
comprimir :: String -> String
comprimir [] = []
comprimir (x:xs) = processar 1 x xs
  where
    processar count char [] 
      | count > 3 = '!' : show count ++ [char]
      | otherwise = replicate count char
    processar count char (y:ys)
      | y == char = processar (count + 1) char ys
      | count > 3 = '!' : show count ++ [char] ++ processar 1 y ys
      | otherwise = replicate count char ++ processar 1 y ys

-- Questão 21: Implemente uma função que descomprima o texto resultante da função anterior
descomprimir :: String -> String
descomprimir [] = []
descomprimir ('!':xs) = 
  let (numStr, a:resto) = spanDigit xs
      n = strToInt numStr
  in replicate n a ++ descomprimir resto
descomprimir (x:xs) = x : descomprimir xs

-- Função auxiliar para separar os dígitos
spanDigit :: String -> (String, String)
spanDigit [] = ([], [])
spanDigit (x:xs)
  | x >= '0' && x <= '9' = let (ds, rest) = spanDigit xs in (x:ds, rest)
  | otherwise = ([], x:xs)

-- Converte uma string numérica para inteiro
strToInt :: String -> Int
strToInt = foldl (\acc c -> acc * 10 + fromEnum c - fromEnum '0') 0