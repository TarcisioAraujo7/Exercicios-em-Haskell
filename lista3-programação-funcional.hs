import Text.Printf (printf)
import Data.List (sort)

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
            (22233344400, "Elizabeth Webber", 'F', (3,1,1992), "Rua Y, 50", "Campinas", "SP", "999911822", "lizweb@gmail.com"), (88812121212, "Thiago Fritz", 'M', (27,10,1986), "Rua Z, 90", "Aracaju", "SE", "91111999999", "thiagof@gmail.com"),
            (12345678900, "Antônio Pontevedra", 'M', (3,10,1977), "Rua P, 51", "Sao Paulo", "SP", "9953912", "balu@gmail.com"), (88812121211, "Arnaldo Fritz", 'M', (26,10,1966), "Rua Z, 90", "Aracaju", "SE", "91111999912", "arnaldof@gmail.com"),
            (11111111111, "Rubens Naluti", 'M', (3,1,1997), "Rua O, 0", "Campinas", "SP", "991242822", "rubens@gmail.com"),     (22222222222, "Joui Jouki", 'M', (27,10,1996), "Rua Z, 91", "Sao Cristovao", "SE", "91111999933", "joui@gmail.com"),
            (33333333333, "Dante", 'M', (12,1,1993), "Rua Y, 50", "Salvador", "BA", "999123422", "dante@gmail.com"),            (44444444444, "Carina Leone", 'F', (27,10,1998), "Rua X, 90", "Salvador", "BA", "91223449999", "leone@gmail.com")]

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
    (12345678900,[("Pfizer", (12,07,2021)),("Pfizer", (12,09,2021))]),
    (11223344556, [("Astrazeneca", (8,08,2021)),("Astrazeneca", (8,10,2021))]),
    (11111111111,[("CoronaVac", (12,07,2021))]),
    (22222222222,[("Pfizer", (12,07,2021))]),
    (88812121212,[("CoronaVac", (30,07,2021)), ("CoronaVac", (30,09,2021))]),
    (99999888887, [("Astrazeneca", (2,08,2021)),("Astrazeneca", (20,09,2021))]),
    (44444444444,[("Astrazeneca", (2,08,2021)),("Astrazeneca", (20,09,2021))]),
    (33333333333,[("Janssen", (2,08,2021)),("Janssen", (2,08,2021))])
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

--Cadastro Demografico
cadastroDemo :: PopPais
cadastroDemo = [
    ("SP",[("Sao Paulo",[((0,10), 100), ((11,20), 150), ((21,30), 250), ((31,40), 200), ((41,50), 250), ((51,60), 200), ((61,70), 150), ((71,80), 100), ((81, 90), 100), ((91,100), 100), ((101,110), 100), ((111,120), 50), ((121,130), 50)]),
           ("Campinas",[((0,10), 50), ((11,20), 150), ((21,30), 150), ((31,40), 100), ((41,50), 130), ((51,60), 200), ((61,70), 50), ((71,80), 70), ((81, 90), 100), ((91,100), 50), ((101,110), 30), ((111,120), 50), ((121,130), 50)])]),
    ("SE",[("Aracaju",[((0,10), 100), ((11,20), 175), ((21,30), 200), ((31,40), 150), ((41,50), 200), ((51,60), 175), ((61,70), 150), ((71,80), 100), ((81, 90), 50), ((91,100), 50), ((101,110), 50), ((111,120), 30), ((121,130), 15)]),
           ("Muribeca",[((0,10), 20), ((11,20), 100), ((21,30), 130), ((31,40), 120), ((41,50), 100), ((51,60), 125), ((61,70), 20), ((71,80), 10), ((81, 90), 0), ((91,100), 10), ((101,110), 20), ((111,120), 30), ((121,130), 0)])]),
    ("BA",[("Salvador",[((0,10), 100), ((11,20), 175), ((21,30), 200), ((31,40), 150), ((41,50), 200), ((51,60), 175), ((61,70), 150), ((71,80), 100), ((81, 90), 50), ((91,100), 50), ((101,110), 50), ((111,120), 30), ((121,130), 15)]),
           ("Feira de Santana",[((0,10), 10), ((11,20), 50), ((21,30), 80), ((31,40), 60), ((41,50), 50), ((51,60), 95), ((61,70), 10), ((71,80), 5), ((81, 90), 1), ((91,100), 2), ((101,110), 10), ((111,120), 30), ((121,130), 0)])])]

--2
--A)
listaEstadoPerc :: PopPais -> Estado -> Vacinados -> CadastroSUS -> IO ()
listaEstadoPerc ps estx vs cs = putStrLn
 (foldr (++) [] [ "\nESTADO:", show estx, "\n",
  "Cidades                      1° Dose       2° Dose\n",
  formataLinha (info ps estx vs cs)
 ])

--Funções auxiliares
formataLinha :: [(Municipio, (String , String ))] -> String
formataLinha [] = ""
formataLinha ((mun,(p1,p2)): ms) = show mun ++ concat (replicate ajuste " ") ++ show p1 ++ concat (replicate ajuste1 " ") ++show p2 ++ "\n" ++ formataLinha ms
        where ajuste = 50 - (length (show mun) + length (show p1 ++ show p2) + (21 - length (show p1 ++ show p2)))
              ajuste1 = 21 - length (show p1 ++ show p2)
              
info :: PopPais -> Estado -> Vacinados -> CadastroSUS -> [(Municipio, (String , String ))]
info ps estx vs cs = zip (cidades (todosMun1 ps estx)) (map (duasPorcentagens vs cs ps estx) (cidades (todosMun1 ps estx)))

duasPorcentagens :: Vacinados -> CadastroSUS -> PopPais -> Estado -> Municipio -> (String, String)
duasPorcentagens vs cs ps estx mun = (porcentagem (vacinados1Total) (cidadeTotal), porcentagem (vacinados2Total) (cidadeTotal))
        where vacinados1Total = fromIntegral (quantidadeDoseMun vs 1 mun cs) :: Float
              vacinados2Total = fromIntegral (quantidadeDoseMun vs 2 mun cs) :: Float
              cidadeTotal =     fromIntegral (totalMunEsp (todosMun1 ps estx) mun) :: Float

totalMunEsp :: [PopMun] -> Municipio -> Quantidade
totalMunEsp [] _ = 0
totalMunEsp ((cidade,[]): cs) munx = 0
totalMunEsp ((cidade,(faixa,quantidade): fs): cs) munx
    | munx == cidade = quantidade + totalMunEsp ((cidade, fs): cs) munx
    | otherwise = totalMunEsp cs munx


totalMun :: PopMun -> Quantidade
totalMun (cidade,[]) = 0
totalMun (cidade,(faixa, quant): fs) = quant + totalMun (cidade, fs)

cidades :: [PopMun] -> [Municipio]
cidades [] = []
cidades ((cidade,pop):xs) = sort (cidade : cidades xs) 

todosMun1 :: PopPais -> Estado ->  [PopMun]
todosMun1 ps estx = selecPopMun (head (filter selecEst ps))
        where selecEst (est,_) = est == estx
              selecPopMun :: (Estado, [PopMun]) -> [PopMun]
              selecPopMun (est,popmun) = popmun

porcentagem ::  Float -> Float -> String
porcentagem _ 0 = ""
porcentagem quantx quanty = printf "%.2f"  (quantx * 100 / quanty) ++ "%"

--B) 

listaVacPais :: PopPais -> CadastroSUS -> Vacinados -> IO ()
listaVacPais ps cs vs = putStrLn (
 "\nPorcentagem de totalmente imunizados por estado.\nEstados                    Totalmente imunizados\n" ++
 formataLinha1 (ordInsercao (info1 ps cs vs)))

--Funções auxiliares
formataLinha1 :: [(Estado,String)] -> String
formataLinha1 [] = ""
formataLinha1 ((est,porcentagem): ms) = show est ++ concat (replicate ajuste " ") ++ show porcentagem ++ "\n" ++ formataLinha1 ms
        where ajuste = 48 - (length (show est) + length (show porcentagem) )

ordInsercao :: [(Estado,String)] -> [(Estado,String)]
ordInsercao [] = []
ordInsercao (x:xs) = insOrd (ordInsercao xs) x

insOrd :: [(Estado,String)] -> (Estado,String) -> [(Estado,String)] 
insOrd [] x = [x]
insOrd ((estx,porcentagemx):xs) (esty,porcentagemy) 
        | (read (init porcentagemx) :: Float) >=  (read (init porcentagemy) :: Float) = (estx,porcentagemx) : (esty,porcentagemy) : xs
        | otherwise = (esty,porcentagemy) : insOrd  xs (estx,porcentagemx)

info1 :: PopPais -> CadastroSUS -> Vacinados -> [(Estado,String)]
info1 ps cs vs = zip (listaEstados ps) (map (porcentagemEstado ps cs vs 2) (listaEstados ps))

listaEstados :: PopPais -> [Estado]
listaEstados [] = []
listaEstados ((est,dados): ps) = est : listaEstados ps

porcentagemEstado :: PopPais -> CadastroSUS -> Vacinados -> TipoDose -> Estado  -> String
porcentagemEstado ps cs vs dose estx = porcentagem a b
        where a = fromIntegral (quantidadeDoseEst vs dose estx cs) :: Float
              b = fromIntegral (somaTotal (todosEst ps estx)) :: Float

todosEst :: PopPais -> Estado -> [[(FaixaIdade, Populacao)]]
todosEst ps estx = head (map selecMun [filter selecEst ps])
        where selecEst (est,_) = est == estx
              selecMun :: [(Estado, [PopMun])] -> [[(FaixaIdade, Populacao)]]
              selecMun [] = []
              selecMun [(est,[])] = []
              selecMun [(est,m:ms)] = snd m : selecMun [(est,ms)]

quantidadeDoseEst :: Vacinados -> TipoDose -> Estado -> CadastroSUS -> Quantidade
quantidadeDoseEst vs 1 estx cadastro = length ( filter (checaEst cadastro estx) vs )
quantidadeDoseEst vs 2 estx cadastro = length ( filter (checaEst cadastro estx) (filter checaSegDose vs))
        where checaSegDose :: Vacinado -> Bool
              checaSegDose (cpf,[(_,_),segdose]) = True
              checaSegDose (cpf,[(_,_)]) = False
quantidadeDoseEst vs _ estx cadastro = error "Informe um tipo de dose valido (1 ou 2)"

somaTotal :: [[(FaixaIdade, Populacao)]] -> Quantidade
somaTotal [[]] = 0
somaTotal [] = 0
somaTotal ([]:ls) = 0 + somaTotal ls
somaTotal (((faixa, pop):xs):ls) = pop + somaTotal (xs:ls)

--C)

quantosFaltamPais ::  PopPais -> CadastroSUS -> Vacinados -> IO ()
quantosFaltamPais ps cs vs =  putStrLn
 (foldr (++) [] [ "\nPopulação não completamente imunizada por estado\n",
  "Estados                              Porcentagem\n",
  formataLinha1 (info2 ps cs vs)
 ]) 

porcentagemF ::  Float -> Float -> String
porcentagemF _ 0 = ""
porcentagemF quantx quanty = printf "%.2f"  (100.0 - (quantx * 100 / quanty)) ++ "%"

info2 :: PopPais -> CadastroSUS -> Vacinados -> [(Estado,String)]
info2 ps cs vs = zip (listaEstados ps) (map (porcentagemEstadoF ps cs vs 2) (listaEstados ps))

porcentagemEstadoF :: PopPais -> CadastroSUS -> Vacinados -> TipoDose -> Estado  -> String
porcentagemEstadoF ps cs vs dose estx = porcentagemF a b
        where a = fromIntegral (quantidadeDoseEst vs dose estx cs) :: Float
              b = fromIntegral (somaTotal (todosEst ps estx)) :: Float

--
listaEstadoFaltam :: PopPais -> Estado -> Vacinados -> CadastroSUS -> IO ()
listaEstadoFaltam ps estx vs cs = putStrLn
 (foldr (++) [] [ "\nPopulação que ainda não foi vacinada por municipio\nESTADO:", show estx, "\n",
  "Cidades                      1° Dose       2° Dose\n",
  formataLinha (info3 ps estx vs cs)
 ])

info3 :: PopPais -> Estado -> Vacinados -> CadastroSUS -> [(Municipio, (String , String ))]
info3 ps estx vs cs = zip (cidades (todosMun1 ps estx)) (map (duasPorcentagensF vs cs ps estx) (cidades (todosMun1 ps estx)))

duasPorcentagensF :: Vacinados -> CadastroSUS -> PopPais -> Estado -> Municipio -> (String, String)
duasPorcentagensF vs cs ps estx mun = (porcentagemF (vacinados1Total) (cidadeTotal), porcentagemF (vacinados2Total) (cidadeTotal))
        where vacinados1Total = fromIntegral (quantidadeDoseMun vs 1 mun cs) :: Float
              vacinados2Total = fromIntegral (quantidadeDoseMun vs 2 mun cs) :: Float
              cidadeTotal =     fromIntegral (totalMunEsp (todosMun1 ps estx) mun) :: Float