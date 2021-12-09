{-Considere uma lista de triplas do tipo [(Int, Int, Int)]. Elabore uma função que
dado uma lista de triplas, devolve uma lista de triplas, com os seus elementos ordenados em
ordem crescente. Por exemplo, se a entrada for [(2,1,5), (4,4,8)] a função
devolverá [(1,2,5), (4,4,8)].Nesta questão é proibido o uso de compreensão e
recursão.-}

ordenaLista :: [(Int,Int,Int)] ->  [(Int,Int,Int)]
ordenaLista xs = map ordenar xs

ordenar :: (Int,Int,Int) -> (Int,Int,Int)
ordenar (x,y,z)
    | x <= y && y <= z = (x,y,z)
    | x <= z && z <= y = (x,z,y)
    | z <= x && x <= y = (z,x,y)
    | y <= x && x <= z = (y,x,z)
    | z <= y && y <= x = (z,y,x)
    | otherwise =        (y,z,x)

{-Considere as seguintes declarações de tipo
type Point = (Double, Double)
data Figura = Circulo {posCirculo :: Point, raio :: Double} |

Retangulo {posRetangulo :: Point,
base :: Double,
altura:: Double} |
FiguraComposta [Figura]

cujos valores representam figuras. No caso do Circulo e Retangulo, o campo de tipo
Point indica a posição do centro da figura.

Defina uma função que dada uma Figura, escale a figura por uma certa quantidade. Assim,
por exemplo, se escalamos um círculo de raio 2 em 3 unidades, o círculo resultante terá raio 6.
Ao escalar uma figura composta, terão que ser escaladas todas as figuras básicas que formam
a figura.-}

type Point = (Double, Double)
data Figura = Circulo{posCirculo :: Point,
                      raio :: Double } |
              Retangulo{posRetangulo :: Point,
                        base :: Double,
                        altura :: Double} 

escalaFigura :: Double -> Figura -> Figura
escalaFigura n figura@Circulo {posCirculo = xy, raio = r }  =  Circulo {posCirculo = xy, raio = r * n }
escalaFigura n figura@Retangulo {posRetangulo = xy, base = b, altura = h }  = Retangulo {posRetangulo = xy, base = b * n, altura = h * n }


{-Prove que:
product (xs ++ reverse ys) = product xs * product ys
considerando as seguintes definições de funções:
product [] = 1 (p.1)

product (y:ys) = y * product ys (p.2)
[] ++ zs = zs (++.1)
(w:ws) ++ zs = w:(ws++zs) (++.2)
reverse [] = [] (r.1)
reverse (w:ws) = reverse ws ++ [w] (r.2)-}

confirmacao :: [Int] -> [Int] -> Bool
confirmacao xs ys = equacao1 xs ys == equacao2 xs ys

equacao1 :: [Int] -> [Int] -> Int
equacao1 xs ys = product xs * product ys

equacao2 :: [Int] -> [Int] -> Int
equacao2 xs ys = product (xs ++ reverse ys )
