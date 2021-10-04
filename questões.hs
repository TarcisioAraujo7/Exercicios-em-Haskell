{-Escreva uma função que recebe um valor de salário atual e retorna o salário novo,
com aumento. O aumento salarial segue a seguinte tabela.
Salário atual Aumento
<1500,00 12%
>=1500,00 e <2000,00 8%
>=2000,00 e < 5000,00 6%
>= 5000,00 5%
-}

calculaAumento :: Int -> Int
calculaAumento salario
    | salario < 1500 = salario + (div salario 100) * 12
    | salario >= 1500 && salario < 2000 = salario + (div salario 100) * 8
    | salario >= 2000 && salario < 5000 = salario + (div salario 100) * 6
    | salario >= 5000 = salario + (div salario 100) * 5


{-Considere uma lista de tuplas em que o primeiro elemento da tupla é o nome de
uma pessoa, o segundo é o gênero, o terceiro o ano de nascimento e o quarto o
estado civil. O gênero admite os valores ‘F’, ‘M’ e ‘X’, os quais denotam,
respectivamente, os gêneros feminino, masculino e demais gêneros. O estado civil
dmite os valores ‘C’, ‘S’, ‘V’ e ‘O’, denotando, respectivamente, os estados civis de
casado, solteiro, viúvo e outros estados civis. Declare tipos para todos os dados e
elabore uma função para receber essa lista de tuplas e o ano corrente, e retornar
uma lista, em que cada posição i, 1≤ i ≤12, indica a quantidade de pessoas com
idade entre (i-1)*10 e i*10 - 1. Você pode assumir que as idades das pessoas na lista   --(i-1)*10 e i*10 - 1 --[0-9,10-19,20-29,30-39,40-49,50-59,60-69,70-79,80-89,90-99,100-109,110-119]
variam de 0 a 119 anos. -}

type Pessoas = [Pessoa]
type Pessoa = (Nome, Genero, AnoNasc, EstadoCivil)
type Nome = String 
type Genero = Char 
type AnoNasc = Int
type EstadoCivil = Char 

lista :: Pessoas
lista = [
    ("Pedro", 'M', 2004, 'S'),
    ("Ana", 'F', 1982, 'C'),
    ("José", 'M', 1999, 'S'),
    ("Joana", 'F', 1960, 'V'),
    ("Mariana", 'F', 2019, 'S')]

listaIdades :: Pessoas -> Int -> [Int]
listaIdades pessoas anoatual = 
    [length ([(nome, gen, ano, est)| (nome, gen, ano, est) <- pessoas, (anoatual-ano) >= 0 && (anoatual-ano) <= 9  ])] ++
    [length ([(nome, gen, ano, est)| (nome, gen, ano, est) <- pessoas, (anoatual-ano) >= 10 && (anoatual-ano) <= 19  ])] ++
    [length ([(nome, gen, ano, est)| (nome, gen, ano, est) <- pessoas, (anoatual-ano) >= 20 && (anoatual-ano) <= 29  ])] ++
    [length ([(nome, gen, ano, est)| (nome, gen, ano, est) <- pessoas, (anoatual-ano) >= 30 && (anoatual-ano) <= 39  ])] ++
    [length ([(nome, gen, ano, est)| (nome, gen, ano, est) <- pessoas, (anoatual-ano) >= 40 && (anoatual-ano) <= 49  ])] ++
    [length ([(nome, gen, ano, est)| (nome, gen, ano, est) <- pessoas, (anoatual-ano) >= 50 && (anoatual-ano) <= 59  ])] ++
    [length ([(nome, gen, ano, est)| (nome, gen, ano, est) <- pessoas, (anoatual-ano) >= 60 && (anoatual-ano) <= 69  ])] ++
    [length ([(nome, gen, ano, est)| (nome, gen, ano, est) <- pessoas, (anoatual-ano) >= 70 && (anoatual-ano) <= 79  ])] ++
    [length ([(nome, gen, ano, est)| (nome, gen, ano, est) <- pessoas, (anoatual-ano) >= 80 && (anoatual-ano) <= 89  ])] ++
    [length ([(nome, gen, ano, est)| (nome, gen, ano, est) <- pessoas, (anoatual-ano) >= 90 && (anoatual-ano) <= 99  ])] ++
    [length ([(nome, gen, ano, est)| (nome, gen, ano, est) <- pessoas, (anoatual-ano) >= 100 && (anoatual-ano) <= 109  ])] ++
    [length ([(nome, gen, ano, est)| (nome, gen, ano, est) <- pessoas, (anoatual-ano) >= 110 && (anoatual-ano) <= 119  ])]

{-Elabore uma função semOcorrencias tal que dados um caractere p e uma lista de
palavras ps, retorna True se nenhuma palavra de ps inicia com p; caso contrário,
retorna False. Por exemplo,
semOcorrencias ‘a’ [“arara”, “gato”, “lebre”, “asno”] devolverá
False e
semOcorrencias ‘c’ [“arara”, “gato”, “lebre”, “asno”] devolverá
True-}

semOcorrencias :: Char -> [String] -> Bool 
semOcorrencias letra lista
    |  [ palavra | (palavra) <- lista, head palavra == letra] == [] = True 
    | otherwise = False
