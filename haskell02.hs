-- Questão 01: Usando compreensão de listas, forneça uma expressão que calcula a soma 1² +2²+...+100² dos quadrados dos primeiros 100 números inteiros
main :: IO ()
main = print (sum[x^2 | x <- [1 .. 100]])

-- Questão 02: Suponha que um plano de coordenadas de tamanho m x n é dado pela lista de todos os pares (x,y) de inteiros tal que 0 menor/igual x menor/igual m e 0 menor/igual y menor/igual n. Usando compreensão de listas, defina a função grid :: Int -> Int -> [(Int,Int)] que retorna o plano de coordenadas de um dado tamanho
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- Questão 3: Usando compreensão de listas e a função grid definida na questão anterior, defina uma função quadrado :: Int -> [(Int,Int)] que retorna um plano de coordenadas quadrado de tamanho n, excluindo a diagonal principal (0,0) a (n,n)
quadrado :: Int -> [(Int,Int)]
quadrado n = [(x, y) | (x, y) <- grid n n, x /= y]

-- Questão 4: De maneira similar à função lenght, mostre como a função replicate :: Int -> a -> [a] que produz uma lista de elementos idênticos pode ser definida usando compreensão de listas.
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- Questão 5: Uma tupla (x,y,z) de inteiros positivos é Pitagoreana se satisfaz a equação x² + y² = z². Usando compreensão de listas com três geradores, defina a função pitag :: Int -> [(Int, Int, Int)] que retorna uma lista de todas as tuplas que satisfazem a condição estabelecida e cujos componentes são menores ou iguais a um dado limite.
pitag :: Int -> [(Int, Int, Int)]
pitag n = [(x, y, z) | x <- [1..n], y <- [x..n], z <- [1..n], x^2 + y^2 == z^2]

-- Questão 6: Um inteiro positivo é perfeito se ele é igual à soma de todos os seus fatores, excluindo o próprio número. Usando compreensão de listas e a função fatores, defina a função perfeitos :: Int -> [Int] que retorna a lista de todos os números perfeitos menores que um limite informado como argumento.
fatores :: Int -> [Int]
fatores n = [x | x <- [1..n-1], n `mod` x == 0]

perfeitos :: Int -> [Int]
perfeitos limite = [n | n <- [2..limite-1], sum (fatores n) == n]

-- Questão 7: Mostre que a compreensão de lista [(x,y) | x <- [1,	2], y <- [3,4]], com dois geradores, pode ser representada usando duas compreensões de lista, cada uma com apenas um gerador. Dica: Procure usar a função concat
comp :: IO()
comp = do
     let resultado = concat [[(x, y) | y <- [3, 4]] | x <- [1,2]]
     print resultado

-- Questão 8: Redefina a função posicoes usando a função buscar
buscar :: Eq a => a -> [(a,b)] -> [b]
buscar k xs = [v | (k', v) <- xs, k == k']

posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = buscar x (zip xs [0..])

-- Questão 9: Escreva a função capaz de calcular o produto escalar de duas listas de inteiros xs e ys de tamanho n, que é dado pelo produto dos inteiros em posições correspondentes. Dica Procure usar a função zip
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar xs ys = sum [x * y | (x, y) <- zip xs ys]