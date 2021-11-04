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
cadastro = [(26716347665, "Paulo Souza", 'M' , (11,10,1996),"Rua A, 202", "Muribeca", "SE", "999997000", "psouza@gmail.com"),   (12345678901, "Ana Reis", 'X' , (5,4,2000), "Rua B, 304", "Aracaju", "SE", "999826004", "areis@gmail.com"),
            (11223344556, "Cesar Oliveira", 'M', (5,1,1990), "Rua X, 18", "Sao Paulo", "SP", "981817070", "ccohen@gmail.com"),  (99999888887, "Arthur Cervero", 'M', (22,4,1992), "Rua X, 19", "Sao Paulo", "SP", "989891010", "acervero@gmail.com"),
            (22233344400, "Elizabeth Webber", 'F', (3,1,1992), "Rua Y, 50", "Campinas", "SP", "999911822", "lizweb@gmail.com"), (88812121212, "Thiago Fritz", 'M', (27,10,1986), "Rua Z, 90", "Aracaju", "SE", "91111999999", "thiagof@gmail.com")]

--1) funções refeitas sem copreensão de lista

--A
atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS
atualizaEndSUS _ [] _ = []
atualizaEndSUS cpfx ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) endnovo
    | cpfx == cpf = (cpf,nome,gen,datanasc,endnovo,cidade,est,num,email) : xs
    | otherwise = (cpf,nome,gen,datanasc,end,cidade,est,num,email) : atualizaEndSUS cpfx xs endnovo

--B
removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS _ [] = []
removeSUS cpfx ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs)
    | cpfx == cpf = xs
    | otherwise = (cpf,nome,gen,datanasc,end,cidade,est,num,email) : removeSUS cpfx xs

--C
geraListaMunicipioFaixas :: CadastroSUS -> Municipio -> Data -> [FaixaIdade] -> [(FaixaIdade, Quantidade)]
geraListaMunicipioFaixas [] _ _ _ = []
geraListaMunicipioFaixas _ _ _ [] = []
geraListaMunicipioFaixas (x:xs) munx datax (y:ys) = (y, cidadaosPorMunicipio (x:xs) munx  y datax ) : geraListaMunicipioFaixas (x:xs) munx datax ys

--Funções Auxiliares
cidadaosPorMunicipio :: CadastroSUS -> Municipio -> FaixaIdade -> Data -> Quantidade
cidadaosPorMunicipio [] _ _ _= 0
cidadaosPorMunicipio ((cpf,nome,gen,datanasc,end,mun,est,num,email):xs) munx faixa datax
    | mun == munx && checaIdade (idadeAtual datanasc datax) faixa =  1 + cidadaosPorMunicipio xs munx faixa datax
    | otherwise = cidadaosPorMunicipio xs munx faixa datax

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

--Banco de Vacinados
cadastroVac :: Vacinados
cadastroVac = [
    (22233344400,[("Pfizer", (12,07,2021))]),
    (11223344556, [("Astrazeneca", (8,08,2021)),("Astrazeneca", (26,09,2021))]),
    (88812121212,[("CoronaVac", (30,07,2021))]),
    (99999888887, [("Astrazeneca", (2,08,2021)),("Astrazeneca", (20,09,2021))])
    ]

--D
aplicaPrimDose :: CPF -> CadastroSUS -> FaixaIdade -> Municipio -> Vacina -> Data -> Vacinados -> Vacinados
aplicaPrimDose cpfx ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) faixax munx vacinax datax ((cpfv, vacinas ): vs)
    | checaCPF cpfx ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) == False = error "O cpf informado não esta cadastrado no sistema."
    | checaVac cpfx ((cpfv, vacinas ): vs) == True = error "A primeira dose ja foi aplicada no usuário."
    | checaResidencia cpfx ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) munx == False = error "A vacinação só é permitida para residentes do município informado, atualize o cadastro do usuário."
    | checaIdade (idadeAtual datanasc datax) faixax == False = error "O usuário nao esta na faixa de idade inserida."
    | vacinax == "Jassen" = (cpfx,[(vacinax,datax),(vacinax,datax)]) : ((cpfv, vacinas ): vs)
    | otherwise = (cpfx,[(vacinax,datax)]) : ((cpfv, vacinas ): vs)

--Funções Auxiliares
checaCPF :: CPF -> CadastroSUS -> Bool
checaCPF _ [] = False
checaCPF cpfx ((cpf,_,_,_,_,_,_,_,_):xs)
    | cpfx == cpf = True
    | otherwise = checaCPF cpfx xs

checaVac :: CPF -> Vacinados -> Bool
checaVac _ [] = False
checaVac cpfx ((cpfv,vacinas): vs)
    | cpfv == cpfx = True
    | otherwise = checaVac cpfx vs

--E
quantidadeDoseMun :: Vacinados -> TipoDose -> Municipio -> CadastroSUS -> Quantidade
quantidadeDoseMun [] _ _ _ = 0
quantidadeDoseMun _ _ _ [] = error "Erro inesperado"
quantidadeDoseMun ((cpf,vacinas):vs) tipodose munx cadastro
    | tipodose >= 3 || tipodose <= 0 = error "Insira uma dose valida"
    | checaResidencia cpf cadastro munx == False = 0 + quantidadeDoseMun vs tipodose munx cadastro
    | tipodose == 1 && tomouPrimDose ((cpf,vacinas):vs) cpf =  1 + quantidadeDoseMun vs tipodose munx cadastro
    | tipodose == 2 && tomouSegDose ((cpf,vacinas):vs) cpf = 1 + quantidadeDoseMun vs tipodose munx cadastro
    | otherwise = quantidadeDoseMun vs tipodose munx cadastro

--Funções Auxiliares
tomouPrimDose :: Vacinados -> CPF -> Bool
tomouPrimDose [] _ = False
tomouPrimDose ((cpf,_):vs) cpfx
    | cpf == cpfx = True
    | otherwise = tomouPrimDose vs cpfx

tomouSegDose :: Vacinados -> CPF -> Bool
tomouSegDose [] _ = False
tomouSegDose ((cpf,[(vacina1)]):vs) cpfx = tomouSegDose vs cpfx
tomouSegDose ((cpf,[_,_]):vs) cpfx
    | cpf == cpfx = True
    | otherwise = tomouSegDose vs cpfx

quantosSegDose :: Vacinados -> Quantidade
quantosSegDose [] = 0
quantosSegDose ((cpf,[(primDose)]):vs) = 0 + quantosSegDose vs
quantosSegDose ((cpf,[(primDose),(segDose)]):vs) = 1 + quantosSegDose vs

checaResidencia :: CPF -> CadastroSUS -> Municipio -> Bool
checaResidencia _ [] _ = False
checaResidencia cpfx ((cpf,_,_,_,_,mun,_,_,_):xs) munx
    | cpf == cpfx && mun == munx = True
    | otherwise = checaResidencia cpfx xs munx

--F
quantidadeEstIdDose :: Vacinados -> Estado -> FaixaIdade -> TipoDose -> Data -> CadastroSUS -> Quantidade
quantidadeEstIdDose [] _ _ _ _ _ = 0
quantidadeEstIdDose _ _ _ _ _ [] = 0
quantidadeEstIdDose ((cpf,vacinas):vs) estx faixax tipodose datax cadastro
    | tipodose >= 3 || tipodose <= 0 = error "Insira uma dose valida"
    | checaEstado cpf cadastro estx == False = 0 + quantidadeEstIdDose vs estx faixax tipodose datax cadastro
    | checaIdade (idadeAtual (coletaData cpf cadastro) datax) faixax == False = 0 + quantidadeEstIdDose vs estx faixax tipodose datax cadastro
    | tipodose == 1 && tomouPrimDose ((cpf,vacinas):vs) cpf = 1 + quantidadeEstIdDose vs estx faixax tipodose datax cadastro
    | tipodose == 2 && tomouSegDose ((cpf,vacinas):vs) cpf = 1 + quantidadeEstIdDose vs estx faixax tipodose datax cadastro
    | otherwise = quantidadeEstIdDose vs estx faixax tipodose datax cadastro

--Funções Auxiliares
checaEstado :: CPF -> CadastroSUS -> Estado -> Bool
checaEstado _ [] _ = False
checaEstado cpfx ((cpf,_,_,_,_,_,est,_,_):xs) estx
    | cpf == cpfx && est == estx = True
    | otherwise = checaEstado cpfx xs estx

coletaData :: CPF -> CadastroSUS -> DataNasc
coletaData _ [] = (error "Não existe no cadastro")
coletaData cpfx ((cpf,_,_,datax,_,_,_,_,_):xs)
    | cpfx == cpf = datax
    | otherwise = coletaData cpfx xs

--G
quantidadeEstVacDose :: Vacinados -> Estado -> Vacina -> TipoDose -> CadastroSUS -> Quantidade
quantidadeEstVacDose [] _ _ _ _ = 0
quantidadeEstVacDose _ _ _ _ [] = 0
quantidadeEstVacDose ((cpf,vacinas):vs) estx vacinax tipodose cadastro
    | tipodose >= 3 || tipodose <= 0 = error "Insira uma dose valida"
    | checaEstado cpf cadastro estx == False = 0 + quantidadeEstVacDose vs estx vacinax tipodose cadastro
    | checaVacina ((cpf,vacinas):vs) vacinax cpf == False = 0 + quantidadeEstVacDose vs estx vacinax tipodose cadastro
    | tipodose == 1 && tomouPrimDose ((cpf,vacinas):vs) cpf = 1 + quantidadeEstVacDose vs estx vacinax tipodose cadastro
    | tipodose == 2 && tomouSegDose ((cpf,vacinas):vs) cpf = 1 + quantidadeEstVacDose vs estx vacinax tipodose cadastro
    | otherwise = quantidadeEstVacDose vs estx vacinax tipodose cadastro

checaVacina :: Vacinados -> Vacina -> CPF -> Bool
checaVacina [] _ _ = False
checaVacina ((cpf,vacinados):vs) vacx cpfx
    | cpf == cpfx && fst(head vacinados) == vacx = True
    | otherwise = checaVacina vs vacx cpfx

--2)


proximaFaixa :: CadastroSUS -> Int -> Municipio -> Quantidade -> Data -> FaixaIdade
proximaFaixa ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) idadex munx quantidadeVac datax = (idadex-1, pegaIdadeMin quantidadeVac ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) idadex munx datax )


somaIdades :: Quantidade -> CadastroSUS -> Int -> Municipio -> Data -> Int
somaIdades _ [] _ _ _ = 0
somaIdades 0 _ _ _ _ = 0
somaIdades _ _ 0 _ _ = 0
somaIdades quantVac ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) idadeMax munx datax
    | quantVac >= cidadaosPorMunicipio ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) munx (idadeMax -1 ,idadeMax -1) datax = cidadaosPorMunicipio ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) munx (idadeMax -1 ,idadeMax -1) datax + somaIdades (quantVac - cidadaosPorMunicipio ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) munx (idadeMax -1 ,idadeMax -1) datax) ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) (idadeMax - 1) munx datax
    | otherwise = 0 + somaIdades quantVac xs idadeMax munx datax

pegaIdadeMin ::  Quantidade -> CadastroSUS -> Int -> Municipio -> Data -> Int
pegaIdadeMin _ [] _ _ _ = 0
pegaIdadeMin 0 _ _ _ _ = 0
pegaIdadeMin _ _ 0 _ _ = 0
pegaIdadeMin quantVac ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) idadeMax munx datax
    | somaIdades quantVac ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) idadeMax munx datax < quantVac = 0 + pegaIdadeMin (somaIdades quantVac ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) idadeMax munx datax) ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) idadeMax munx datax
    | quantVac > cidadaosPorMunicipio ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) munx (idadeMax -1 ,idadeMax -1) datax = pegaIdadeMin (quantVac - cidadaosPorMunicipio ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) munx (idadeMax -1 ,idadeMax -1) datax) ((cpf,nome,gen,datanasc,end,cidade,est,num,email):xs) (idadeMax - 1) munx datax
    | otherwise = idadeMax -1

--Cadastro Demografico
cadastroDemografico :: PopPais
cadastroDemografico = [
    ("SP",[("Sao Paulo",[((0,10), 100), ((11,20), 150), ((21,30), 250), ((31,40), 200), ((41,50), 250), ((51,60), 200), ((61,70), 150), ((71,80), 100), ((81, 90), 100), ((91,100), 100), ((101,110), 100), ((111,120), 50), ((121,130), 50)]),
           ("Campinas",[((0,10), 50), ((11,20), 150), ((21,30), 150), ((31,40), 100), ((41,50), 130), ((51,60), 200), ((61,70), 50), ((71,80), 70), ((81, 90), 100), ((91,100), 50), ((101,110), 30), ((111,120), 50), ((121,130), 50)])]),
    ("SE",[("Aracaju",[((0,10), 100), ((11,20), 175), ((21,30), 200), ((31,40), 150), ((41,50), 200), ((51,60), 175), ((61,70), 150), ((71,80), 100), ((81, 90), 50), ((91,100), 50), ((101,110), 50), ((111,120), 30), ((121,130), 15)]),
           ("Muribeca",[((0,10), 25), ((11,20), 100), ((21,30), 130), ((31,40), 120), ((41,50), 100), ((51,60), 125), ((61,70), 20), ((71,80), 10), ((81, 90), 0), ((91,100), 10), ((101,110), 20), ((111,120), 30), ((121,130), 0)])])]

--3)
percentualVacinacao :: CadastroSUS -> Vacinados -> PopPais -> Estado -> TipoDose -> Data -> [(FaixaIdade,String)]
percentualVacinacao (x:xs) (v:vs) (p:ps) estx dosex datax =  (zipando (faixas (separaPopEst2 (p:ps) estx)) (listaPorcentagem (listaVacinados (v:vs) estx (faixas (separaPopEst2 (p:ps) estx)) dosex datax (x:xs)) (quantiasFaixas (p:ps) estx (faixas (separaPopEst2 (p:ps) estx)))))

--Funções Auxiliares

faixas :: [(FaixaIdade, Populacao)] -> [FaixaIdade]
faixas [] = []
faixas (x : xs) = fst x : faixas xs

porcentagem :: Float -> Float -> String
porcentagem quantx quanty = formataPorcentagem (quantx * 100 / quanty)

formataPorcentagem :: Float -> String
formataPorcentagem x = (take 4 (show x)) ++ "%"

faixaEspecifica :: [(FaixaIdade, Populacao)] -> FaixaIdade -> Quantidade
faixaEspecifica [] _ = 0
faixaEspecifica  ((faixa, quant) : fs ) faixax
    | faixa == faixax = quant + faixaEspecifica fs faixax
    | otherwise = faixaEspecifica fs faixax

separaPopEst :: PopPais -> Estado ->  [(FaixaIdade, Populacao)]
separaPopEst [] _ = error "O estado não consta no cadastro"
separaPopEst ( (estado, []) : xs) _ = []
separaPopEst ( (estado, ((mun,(faixas : fs)) : ms )) : xs ) estx
    | ((mun,(faixas : fs)) : ms ) == [] = [] ++ separaPopEst ((estado, ms) : xs) estx
    | null (estado, ((mun,(faixas : fs)) : ms )) = [] ++ separaPopEst  xs estx
    | estado == estx =  (faixas : fs) ++ separaPopEst ( (estado,  ms ) : xs ) estx
    | otherwise = separaPopEst xs estx

quantiasFaixas :: PopPais -> Estado -> [FaixaIdade] -> [Quantidade]
quantiasFaixas [] _ _ = []
quantiasFaixas _ _ [] = []
quantiasFaixas popPais estx (f:fs) = faixaEspecifica (separaPopEst popPais estx) f : quantiasFaixas popPais estx fs

separaPopEst2 :: PopPais -> Estado -> [(FaixaIdade, Populacao)]
separaPopEst2 [] _ = error "O estado não consta no cadastro"
separaPopEst2 ( (estado, ((mun,(faixas : fs)) : ms )) : xs ) estx
    | estado == estx =  faixas : fs
    | otherwise = separaPopEst2 xs estx

zipando :: [FaixaIdade] -> [String] -> [(FaixaIdade,String)]
zipando [] _ = []
zipando _ [] = []
zipando (x : xs) (y : ys) = (x,y) : zipando xs ys

listaVacinados :: Vacinados -> Estado -> [FaixaIdade] -> TipoDose -> Data -> CadastroSUS -> [Quantidade]
listaVacinados _ _ [] _ _ _ = []
listaVacinados (v:vs) estx (f:fs) tipodose datax (x:xs) = quantidadeEstIdDose (v:vs) estx f tipodose datax (x:xs) : listaVacinados (v:vs) estx fs tipodose datax (x:xs)

listaPorcentagem :: [Quantidade] -> [Quantidade] -> [String]
listaPorcentagem [] _ = []
listaPorcentagem _ [] = []
listaPorcentagem (x:xs) (y:ys) = porcentagem a b : listaPorcentagem xs ys
    where a = fromIntegral x :: Float
          b = fromIntegral y :: Float