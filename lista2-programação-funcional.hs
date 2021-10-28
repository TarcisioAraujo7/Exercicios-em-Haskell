type CadastroSUS = [Cidadao]
type CPF = Integer
type Nome = String
type Genero = Char
type Dia = Int
type Mes = Int
type Ano = Int
type Data = (Dia, Mes, Ano)
type DataNasc = Data
type Endereco = String
type Municipio = String
type Estado = String
type Telefone = String
type Email = String
type Cidadao = (CPF, Nome, Genero, DataNasc, Endereco, Municipio, Estado, Telefone, Email)
type IdadeInicial = Int
type IdadeFinal = Int
type FaixaIdade = (IdadeInicial, IdadeFinal)
type Quantidade = Int
type Vacinados = [Vacinado]
type Vacinado = (CPF, Doses)
type Doses = [Dose]
type Dose = (Vacina, Data)
type Vacina = String
type TipoDose = Int

--Banco de dados 

cadastro :: CadastroSUS
cadastro = [(26716347665, "Paulo Souza", 'M' , (11,10,1996),"Rua A, 202", "Muribeca", "SE", "999997000", "psouza@gmail.com"),   (87717347115, "Ana Reis", 'X' , (5,4,2000), "Rua B, 304", "Aracaju", "SE", "999826004", "areis@gmail.com"),
            (11223344556, "Cesar Oliveira", 'M', (5,1,1990), "Rua X, 18", "Sao Paulo", "SP", "981817070", "ccohen@gmail.com"),  (99999888887, "Arthur Cervero", 'M', (22,4,1992), "Rua X, 19", "Sao Paulo", "SP", "989891010", "acervero@gmail.com"),
            (22233344400, "Elizabeth Webber", 'F', (3,1,1992), "Rua Y, 50", "Campinas", "SP", "999911822", "lizweb@gmail.com"), (88812121212, "Thiago Fritz", 'M', (27,10,1986), "Rua Z, 90", "Aracaju", "SE", "91111999999", "thiagof@gmail.com")]

atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS
atualizaEndSUS _ [] _ = []
atualizaEndSUS cpfx ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) endnovo
    | cpfx == cpf = (cpf,nome,gen,datanasc,endnovo,cidade,est,num,email) : xs
    | otherwise = (cpf,nome,gen,datanasc,end,cidade,est,num,email) : atualizaEndSUS cpfx xs endnovo

removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS _ [] = []
removeSUS cpfx ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs)
    | cpfx == cpf = xs
    | otherwise = (cpf,nome,gen,datanasc,end,cidade,est,num,email) : removeSUS cpfx xs

geraListaMunicipioFaixas :: CadastroSUS -> Municipio -> Data -> [FaixaIdade] -> [(FaixaIdade, Quantidade)]
geraListaMunicipioFaixas [] _ _ _ = []
geraListaMunicipioFaixas _ _ _ [] = []
geraListaMunicipioFaixas (x:xs) munx datax (y:ys) = (y, cidadaosPorMunicipio (x:xs) munx  y datax ) : geraListaMunicipioFaixas (x:xs) munx datax ys

cidadaosPorMunicipio :: CadastroSUS -> Municipio -> FaixaIdade -> Data -> Quantidade
cidadaosPorMunicipio [] _ _ _= 0
cidadaosPorMunicipio ((cpf,nome,gen,datanasc,end,mun,est,num,email):xs) munx faixa datax
    | mun == munx && checaIdade (idadeAtual datanasc datax) faixa =  1 + (cidadaosPorMunicipio xs munx faixa datax)
    | otherwise = cidadaosPorMunicipio xs munx faixa datax

checaCPF :: CPF -> CadastroSUS -> Bool
checaCPF _ [] = False
checaCPF cpfx ((cpf,_,_,_,_,_,_,_,_):xs)
    | cpfx == cpf = True 
    | otherwise = checaCPF cpfx xs
 
checaIdade :: Int -> FaixaIdade -> Bool
checaIdade idadex faixax
    |  fst faixax <= idadex && idadex <= snd faixax = True
    | otherwise = False

idadeAtual :: DataNasc -> Data -> Int
idadeAtual (dia, mes, ano) (diaHJ, mesAtual, anoAtual)
    | mes == mesAtual && dia <= diaHJ = anoAtual - ano
    | mes == mesAtual && dia > diaHJ = anoPassado - ano
    | mes > mesAtual = anoPassado - ano
    | mes < mesAtual = anoAtual - ano
    where
        anoPassado = anoAtual - 1


cadastroVac :: Vacinados
cadastroVac = [
    (22233344400,[("Pfizer", (12,07,2021))]),
    (11223344556, [("Astrazeneca", (8,08,2021)),("Astrazeneca", (26,09,2021))]), 
    (88812121212,[("CoronaVac", (30,07,2021))]),
    (99999888887, [("Astrazeneca", (2,08,2021)),("Astrazeneca", (20,09,2021))])
    ]