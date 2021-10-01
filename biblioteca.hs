type Pessoa = String
type Livro = String
type Emprestimos = [(Pessoa, Livro)]

bancoDeDados :: Emprestimos
bancoDeDados = [("Tarcisio","Duna"),("Tarcisio","It"),("Alicia", "It"),("Alicia", "Harry Potter")]

livPessoa :: Pessoa -> [Livro]
livPessoa pessoax = [livro | (pessoa,livro) <- bancoDeDados, pessoa == pessoax ]

livroEmprestado :: Livro -> [Pessoa]
livroEmprestado livrox = [ pessoa | (pessoa, livro) <- bancoDeDados, livro == livrox]

emprestado :: Livro -> Bool
emprestado livrox
    | null ([livro | (pessoa, livro) <- bancoDeDados, livro == livrox]) = False
    | otherwise = True

quantos :: Pessoa -> Int
quantos pessoax = length [livro | (pessoa,livro) <- bancoDeDados, pessoa == pessoax]