-- Questão 1: Como a versão recursiva da função fatorial se comporta se dermos a ela como argumento um número negativo? Modifique a implementação clássica para não permitir números negativos adicionando uma guarda ao passo recursivo
fatorial :: Integer -> Integer
fatorial n
 | n < 0 = error "Fatorial não é definido para números negativos"
 | n == 0  = 1
 | otherwise = n * fatorial (n - 1)

-- Questão 2: Defina a função recursiva somar :: Int -> Int que retorna a soma dos inteiros não-negativos a partir de um valor até zero.
somar :: Int -> Int
somar n
 | n < 0 = error "somar número negativo não permitido"
 | n == 0  = 0
 | otherwise = n + somar (n - 1)

-- Questão 3: Defina o operador de exponenciação ^ utilizando uma função recursiva, semelhante ao padrão usado para implementar a multiplicação com o operador *
expo :: (Num a, Integral b) => a -> b -> a
expo _ 0 = 1
expo m n
  | n < 0     = error "Expoente negativo não suportado"
  | otherwise = m * expo m (n - 1)

-- Questão 4: Defina a função euclides :: Int -> Int -> Int que implementa o algoritmo de Euclides para calcular o máximo divisor comum de dois inteiros não-negativos: se dois números são iguais, este número é o resultado; caso contrário, o menor número é subtraído do maior e o processo é repetido passando este novo número e o menor valor passado anteriormente como argumento.
euclides :: Int -> Int -> Int
euclides a b
 | a == b  = a 
 | a > b   = euclides (a - b) b
 | otherwise = euclides a (b - a)

-- Questão 5: Defina as funções abaixo usando recursão:
-- a) Decidir se todos os valores em uma lista são True:
andRod :: [Bool] -> Bool
andRod []    = True
andRod (x:xs) = x && andRod xs

-- b) Concatenar uma lista de listas:
concatRod :: [[a]] -> [a]
concatRod []     = []
concatRod (x:xs) = x ++ concatRod xs

-- c) Produzir uma lista com n elementos idênticos:
replicateRod :: Int -> a -> [a]
replicateRod n x
  | n <= 0    = []
  | otherwise = x : replicateRod (n - 1) x

-- d) Selecionar o n-ésimo elemento em uma lista:
nesimoRod :: [a] -> Int -> a
nesimoRod (x:_) 0 = x                       
nesimoRod (_:xs) n
  | n < 0     = error "Índice negativo"      
  | otherwise = nesimoRod xs (n - 1)         
nesimoRod [] _ = error "Índice fora da faixa" 

-- e) Decidir se um valor está presente em uma lista:
elemento :: Eq a => a -> [a] -> Bool
elemento _ [] = False
elemento y (x:xs)
  | y == x    = True
  | otherwise = elemento y xs

-- Questão 6: Definir a função recursiva merge :: Ord a => [a] -> [a] -> [a] que une duas listas ordenadas em uma lista ordenada
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
 | x <= y = x: merge xs(y:ys)
 | otherwise = y: merge (x:xs) ys

-- Questão 7: Usando a função merge, defina a função mergesort :: Ord a => [a] -> [a] que implementa o algoritmo de ordenação merge sort, que por sua vez considera uma lista vazia e uma lista com apenas um elemento como listas ordenadas, e que qualquer outra lista é ordenada a partir da união de duas listas que resultaram da ordenação das suas metades separadamente. Dica: primeiro implemente a função metades :: [a] -> ([a],[a]) que separa uma lista em duas partes cujos comprimentos são iguais ou no máximo diferem em uma unidade
metades :: [a] -> ([a], [a])
metades xs = splitAt (length xs `div` 2) xs -- Essa função divide uma lista em duas metades

mergeRod :: Ord a => [a] -> [a] -> [a]
mergeRod [] ys = ys
mergeRod xs [] = xs
mergeRod (x:xs) (y:ys)
  | x <= y    = x : mergeRod xs (y:ys)
  | otherwise = y : mergeRod (x:xs) ys -- Essa função junta duas listas ordenadas em uma lista ordenada

mergesort :: Ord a => [a] -> [a]
mergesort [] = [] -- Lista vazia já está organizada
mergesort [x] = [x] -- Lista com um único elemento já está ordenada
mergesort xs = let (ys, zs) = metades xs
       in mergeRod (mergesort ys) (mergesort zs) -- Essa parte do código age com a função principal de ordenação e implementa o Merge Sort

-- Questão 8: Implemente recursivamente funções que:
-- a) Calcule a soma de uma lista de inteiros
somaRod :: [Int] -> Int
somaRod [] = 0  -- Caso base: A soma de uma lista vazia é 0
somaRod (x:xs) = x + somaRod xs -- Caso recursivo: Soma o primeiro elemento ao número de elementos do resto da lista

-- b) Obtenha o número de elementos de uma lista
comprimento :: [a] -> Int
comprimento [] = 0 -- Caso base: Lista vazia com 0 elementos
comprimento (x:xs) = 1 + comprimento xs -- Caso recursivo: Soma 1 ao número de elementos do resto da lista

-- c) Selecione o último elemento de uma lista não vazia
ultimo :: [a] -> a
ultimo [x] = x -- Caso base: A lista possui apenas um único elemento, então ele é o último elemento dessa lista
ultimo (_:xs) = ultimo xs -- Caso recursivo: Ignora o primeiro elemento e chama recursivamente no resto