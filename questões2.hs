{-
Dada uma lista de palavras, elabore uma função que retorne uma frase em que as
palavras da lista estão separadas por um espaço em branco. O último caractere da
frase é a última letra da última palavra da lista, caso a lista de palavras não seja
vazia. Nesta função você não pode usar a função concat do Prelude. Por exemplo,
para a lista [“boa”, “prova”] a função retornará “boa prova”.
-}


ordenaPalavras :: [String] -> String
ordenaPalavras [] = []
ordenaPalavras (x:xs)
    | ordenaPalavras xs == [] = (x) ++ ordenaPalavras xs
    | otherwise = (x ++ " ") ++ ordenaPalavras xs

{-
Defina uma função que receba uma lista de inteiros ordenados e retorne as
frequências de cada elemento, ou seja, quantas vezes ocorre cada elemento na
lista. O resultado deve ser uma lista de pares onde cada par contém o elemento e o
número de ocorrências na lista de entrada. A lista de saída deve manter a ordem
com relação aos elementos da lista original.
-}

formatFrequenciaInt :: [(Int, Int)] -> [(Int, Int)]
formatFrequenciaInt [] = []
formatFrequenciaInt (x:xs)
    | fst x == fst(head xs) = formatFrequenciaInt (x : tail xs)
    | otherwise = (x:xs)

frequenciaInt :: [Int] -> [(Int, Int)]
frequenciaInt [] = []
frequenciaInt (x:xs) = (x, confereQuantos x (x:xs)) : frequenciaInt xs

confereQuantos :: Int -> [Int] -> Int
confereQuantos _ [] = 0
confereQuantos intx (x:xs)
    | intx == x = 1 + confereQuantos intx xs
    | otherwise = confereQuantos intx xs

{-
Seja L uma lista de listas de inteiros. Chama-se de sublistas as listas que são os
elementos de L. Por exemplo, a lista L dada por [[1,2],[],[2,4]], possui as
sublistas [1,2], [] e [2,4]. Dada uma lista de listas de inteiros, elabore uma
função que devolva uma lista composta das sublistas cujos tamanhos não sejam
iguais ao menor tamanho das sublistas da lista de entrada.
-}

excetoMenor :: [[Int]] -> [[Int]]
excetoMenor [] = []
excetoMenor (x:xs) = listaSemMenor (x:xs) (qualMenor (x:xs))

qualMenor :: [[Int]] -> [Int]
qualMenor [] = []
qualMenor (x:xs)
    | length x <= length (head (xs)) = length x : qualMenor (tail xs)
    | otherwise = [] ++ qualMenor xs

listaSemMenor :: [[Int]] -> [Int] -> [[Int]]
listaSemMenor [] _ = []
listaSemMenor _ [] = []
listaSemMenor (x:xs) y 
    | length (x) == head y = listaSemMenor xs y 
    | otherwise = x : listaSemMenor xs y
