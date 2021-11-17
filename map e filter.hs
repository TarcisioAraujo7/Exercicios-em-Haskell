import Data.Char 
iguais :: Eq a => a -> a -> a -> Bool 
iguais x y z = z == y && y == z 

quadLista :: [Int] -> [Int]
quadLista xs = map quad xs
    where quad x = x ^ 2

codigos :: [Char] -> [Int]
codigos xs = map ord xs

segundos :: [(Int,Int)] -> [Int]
segundos xs = map snd xs 

dobroZW :: [Int] -> [Int]
dobroZW [] = []
dobroZW xs = zipWith (*) xs (replicate (length xs) 2)

quadZW :: [Int] -> [Int]
quadZW [] = []
quadZW xs = zipWith (*) xs xs

temDigito :: String -> Bool
temDigito [] = False
temDigito ps = and (map ehChar ps)

ehChar :: Char -> Bool
ehChar c = 'a' <= c && 'z' >= c || 'A' <= c && 'Z' >= c

naoehChar :: Char -> Bool
naoehChar c = not ('a' <= c && 'z' >= c || 'A' <= c && 'Z' >= c)

temChar :: String -> Bool
temChar [] = False
temChar ps = or (map naoehChar ps)

minusculas :: String -> String
minusculas cs = filter ehMinusculo cs
    where ehMinusculo c = 'a' <= c && 'z' >= c

soPositivos :: [Int] -> [Int]
soPositivos cs = filter ehPos cs
    where ehPos c = c >= 0

paraoQuadrado :: (Num a, Eq a) => [(a,a)] -> [(a,a)]
paraoQuadrado xs = filter ehAoQuadrado xs
   
ehAoQuadrado :: (Num a, Eq a) => (a,a) -> Bool   
ehAoQuadrado x = fst x^2 == snd x

pares :: (Eq a) => [a] -> [a]
pares xs = formataLista
    where listaComIndex = zip [1..] xs
          formataLista = map snd (filter ehPar listaComIndex)

ehPar :: (Eq a) => (Int,a) -> Bool 
ehPar (p,_) = even p
