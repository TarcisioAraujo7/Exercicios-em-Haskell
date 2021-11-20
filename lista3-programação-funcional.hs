import Text.Printf (printf)
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
type Populacao = Int
type PopMun = (Municipio, [(FaixaIdade, Populacao)])
type PopEstado = (Estado, [PopMun])
type PopPais = [PopEstado]

--Banco de dados 

cadastro :: CadastroSUS
cadastro = [(26716347665, "Paulo Souza", 'M' , (11,10,1996),"Rua A, 202", "Muribeca", "SE", "999997000", "psouza@gmail.com"),   (87717347115, "Ana Reis", 'X' , (5,4,2000), "Rua B, 304", "Aracaju", "SE", "999826004", "areis@gmail.com"),
            (11223344556, "Cesar Oliveira", 'M', (5,1,1990), "Rua X, 18", "Sao Paulo", "SP", "981817070", "ccohen@gmail.com"),  (99999888887, "Arthur Cervero", 'M', (22,4,1992), "Rua X, 19", "Sao Paulo", "SP", "989891010", "acervero@gmail.com"),
            (22233344400, "Elizabeth Webber", 'F', (3,1,1992), "Rua Y, 50", "Campinas", "SP", "999911822", "lizweb@gmail.com"), (88812121212, "Thiago Fritz", 'M', (27,10,1986), "Rua Z, 90", "Aracaju", "SE", "91111999999", "thiagof@gmail.com")]

-- 1
--A)
atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS
atualizaEndSUS cpfx cs endx = takeWhile naoCPF cs  ++  mudaEnd (filter checaCPF cs) ++ tail (dropWhile naoCPF cs)
        where checaCPF (cpf,nome,gen,datanasc,end,mun,est,num,email) = cpf == cpfx
              naoCPF (cpf,nome,gen,datanasc,end,mun,est,num,email) = cpf /= cpfx
              mudaEnd :: CadastroSUS -> CadastroSUS
              mudaEnd [(cpf,nome,gen,datanasc,end,mun,est,num,email)] = [(cpf,nome,gen,datanasc,endx,mun,est,num,email)]

--B)
removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS cpfx cs = takeWhile naoCPF cs ++ tail (dropWhile naoCPF cs)
        where checaCPF (cpf,nome,gen,datanasc,end,mun,est,num,email) = cpf == cpfx
              naoCPF (cpf,nome,gen,datanasc,end,mun,est,num,email) = cpf /= cpfx

--C)
geraListaMunicipioFaixas :: CadastroSUS -> Municipio -> Data -> [FaixaIdade] -> [(FaixaIdade, Quantidade)]
geraListaMunicipioFaixas cs munx datax fs = zip fs (map (cidadaosPorMunicipioIdade cs munx datax) fs)

--Funções Auxiliares.
cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio -> Data -> FaixaIdade -> Quantidade
cidadaosPorMunicipioIdade cs munx datax faixax = length (filter confereID (filter confereMUN cs))
        where confereMUN (cpf,nome,gen,datanasc,end,mun,est,num,email) = munx == mun
              confereID (cpf,nome,gen,datanasc,end,mun,est,num,email) = checaIdade (idadeAtual datanasc datax) faixax

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

--Banco de dados dos vacinados.

cadastroVac :: Vacinados
cadastroVac = [
    (22233344400,[("Pfizer", (12,07,2021))]),
    (11223344556, [("Astrazeneca", (8,08,2021))]),
    (88812121212,[("CoronaVac", (30,07,2021))]),
    (99999888887, [("Astrazeneca", (2,08,2021)),("Astrazeneca", (20,09,2021))])
    ]

--D)
quantidadeDoseMun :: Vacinados -> TipoDose -> Municipio -> CadastroSUS -> Quantidade
quantidadeDoseMun vs 1 munx cadastro = length ( filter (checaMun cadastro munx) vs )
quantidadeDoseMun vs 2 munx cadastro = length ( filter (checaMun cadastro munx) (filter checaSegDose vs))
        where checaSegDose :: Vacinado -> Bool
              checaSegDose (cpf,[(_,_),segdose]) = True
              checaSegDose (cpf,[(_,_)]) = False
quantidadeDoseMun vs _ munx cadastro = error "Informe um tipo de dose valido (1 ou 2)"

--Função Auxiliar.
checaMun :: CadastroSUS -> Municipio -> Vacinado -> Bool
checaMun cs munx cid = any checaCPFEND cs
    where checaCPFEND (cpf,nome,gen,datanasc,end,mun,est,num,email) = cpf == fst cid && mun == munx

--E)
quantidadeEstIdDose :: Vacinados -> Estado -> FaixaIdade -> TipoDose -> Data -> CadastroSUS -> Quantidade
quantidadeEstIdDose vs estx faixax 1 datax cs = length (filter (checaIdadeVac cs datax faixax) (filter (checaEst cadastro estx) vs))
quantidadeEstIdDose vs estx faixax 2 datax cs = length (filter checaSegDose (filter (checaIdadeVac cs datax faixax) (filter (checaEst cadastro estx) vs)))
        where checaSegDose :: Vacinado -> Bool
              checaSegDose (cpf,[(_,_),segdose]) = True
              checaSegDose (cpf,[(_,_)]) = False
quantidadeEstIdDose vs estx faixax _ datax cs = error "Informe um tipo de dose valido (1 ou 2)"

--Funções Auxiliares
checaEst :: CadastroSUS -> Estado -> Vacinado -> Bool
checaEst cs estx cid = any checaCPFEST cs
    where checaCPFEST (cpf,nome,gen,datanasc,end,mun,est,num,email) = cpf == fst cid && est == estx

checaIdadeVac :: CadastroSUS -> Data -> FaixaIdade -> Vacinado -> Bool
checaIdadeVac cs datax faixax cid = any confereID cs
            where confereID (cpf,nome,gen,datanasc,end,mun,est,num,email) = checaIdade (idadeAtual datanasc datax) faixax && cpf == fst cid

--F)
quantidadeEstVacDose :: Vacinados -> Estado -> Vacina -> TipoDose -> CadastroSUS -> Quantidade
quantidadeEstVacDose vs estx vac 1 cs = length (filter (comparaDose1 vac) (filter (checaEst cs estx) vs))
quantidadeEstVacDose vs estx vac 2 cs = length (filter (comparaDose2 vac) (filter (checaEst cs estx) vs))
quantidadeEstVacDose vs estx vac _ cs = error "Informe um tipo de dose valido (1 ou 2)"

--Funções Auxiliares.
comparaDose1 ::  Vacina -> Vacinado -> Bool
comparaDose1 vac (cpf,[(vac1,_)])  = vac1 == vac
comparaDose1 vac (cpf,[(_,_),(vac2,_)]) = vac2 == vac

comparaDose2 :: Vacina -> Vacinado -> Bool
comparaDose2 vac (cpf,[(_,_),(vac2,_)]) = vac2 == vac
comparaDose2 vac (cpf,[(_,_)]) = False