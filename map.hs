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
