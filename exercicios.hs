import Data.Char
ordem :: Int -> Int -> Int -> (Int, Int,Int)
ordem x y z
   | x >= y && y >= z = (x, y, z)
   | y >= x && x >= z = (y, x, z)
   | y >= z && z > x = (y, z, x)
   | x >= z && z > y = (x, z, y)
   | z >= x && x > y = (z, x, y)
   | z > y = (z, y, x)

ehImpar :: Int -> Bool
ehImpar x = odd x

listaInt :: [Int] -> [Int]
listaInt lista = [3 * x | x <- lista , ehImpar x]

ehMinusculo :: Char -> Bool
ehMinusculo x = 'a' <= x && 'z' >= x

ehMaiusculo :: Char -> Bool
ehMaiusculo x = 'A' <= x && 'Z' >= x

maiusculo :: String  -> String
maiusculo palavra = [chr (ord n - ord 'a' + ord 'A') | n <- palavra, ehMinusculo n]

pali :: String -> String -> Bool
pali palavra1 palavra2
   | palavra1 == cpalavra1 && palavra2 == cpalavra2 = True
   |otherwise = False
   where cpalavra1 = reverse palavra1
         cpalavra2 = reverse palavra2

soma5 :: [Int] -> Int
soma5 xs = sum[ x | x <- xs, x >= 5]

mediaList :: [Int] -> Int
mediaList hs = div (sum hs) (length hs)

