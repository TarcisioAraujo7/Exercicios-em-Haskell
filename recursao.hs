{- Construa uma função para efetuar o produto dos números de uma lista de inteiros de tamanho n.  
Esta seria a função product aplicada numa lista de inteiros.  -}

producto :: [Int] -> Int
producto [] = 1
producto (x:xs) = x * producto xs