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
type QuantidadeFormatada = String
type LinhaFormatada = String
type LinhasFormatadas = String
type TotalFormatado = String
type FaixaFormatada = String

--Banco de dados 

cadastro :: CadastroSUS
cadastro = [(26716347665, "Paulo Souza", 'M' , (11,10,1996),"Rua A, 202", "Muribeca", "SE", "999997000", "psouza@gmail.com"),   (87717347115, "Ana Reis", 'X' , (5,4,2000), "Rua B, 304", "Aracaju", "SE", "999826004", "areis@gmail.com"),
            (11223344556, "Cesar Oliveira", 'M', (5,1,1990), "Rua X, 18", "Sao Paulo", "SP", "981817070", "ccohen@gmail.com"),  (99999888887, "Arthur Cervero", 'M', (22,4,1992), "Rua X, 19", "Sao Paulo", "SP", "989891010", "acervero@gmail.com"),
            (22233344400, "Elizabeth Webber", 'F', (3,1,1992), "Rua Y, 50", "Campinas", "SP", "999911822", "lizweb@gmail.com"), (88812121212, "Thiago Fritz", 'M', (27,10,1986), "Rua Z, 90", "Aracaju", "SE", "91111999999", "thiagof@gmail.com")]

--Cadastramento de um cidadão no sistema.

adicionaSUS :: Cidadao -> CadastroSUS -> CadastroSUS
adicionaSUS cidadaox cadastro
    | checaCPF cpfcidadao cadastro = cadastro ++ [cidadaox]
    | otherwise = error "O usuário já está cadastrado."
    where cpfcidadao = head [cpf | (cpf,_,_,_,_,_,_,_,_) <- [cidadaox] ]

checaCPF :: CPF -> CadastroSUS -> Bool
checaCPF cpfx cadastro
    | [cpf | (cpf,_,_,_,_,_,_,_,_) <- cadastro, cpf == cpfx] == [] = True
    | otherwise = False


pegaCPF :: Cidadao -> CPF
pegaCPF (cpfx,_,_,_,_,_,_,_,_) = cpfx

--Funções para atualizar o endereço e o número de telefone.

atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS
atualizaEndSUS cpfx cadastro endnovo
    | checaCPF cpfx cadastro == False = parte1 ++ [ (cpf,nome,genero,datanasc,endnovo,cidade,estado,num,email) | (cpf,nome,genero,datanasc,endereco,cidade,estado,num,email) <- cadastro, cpf == cpfx] ++ parte2
    | otherwise = error "Este usuário não está cadastrado."
     where parte1 = take (posi - 1) cadastro
           parte2 = drop posi cadastro
           posi = posicao cpfx cadastro

atualizaTelSUS :: CPF -> CadastroSUS -> Telefone -> CadastroSUS
atualizaTelSUS cpfx cadastro telnovo
    | checaCPF cpfx cadastro == False = parte1 ++ [ (cpf,nome,genero,datanasc,endereco,cidade,estado,telnovo,email) | (cpf,nome,genero,datanasc,endereco,cidade,estado,num,email) <- cadastro, cpf == cpfx] ++ parte2
    | otherwise = error "Este usuário não está cadastrado."
     where parte1 = take (posi - 1) cadastro
           parte2 = drop posi cadastro
           posi = posicao cpfx cadastro

--Função para remover um cidadão do cadastro.

removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS cpfx cadastro
    | checaCPF cpfx cadastro == False = parte1 ++ parte2
    | otherwise = error "Este usuário não está cadastro."
     where parte1 = take (posi - 1) cadastro
           parte2 = drop posi cadastro
           posi = posicao cpfx cadastro

--Funções auxiliares.

idx :: CadastroSUS -> [(Int,Cidadao)]
idx cadastro = zip listaCad cadastro
    where listaCad = [1..(length cadastro)]

posicao :: CPF -> CadastroSUS -> Int
posicao cpfx cadastro
    | lista == [] = 0
    | otherwise = head lista
    where lista = [ pos | (pos, cidadao) <- idx cadastro, cpfx == pegaCPF cidadao ]

--Funções de pesquisa por município e estado.

cidadaosPorMunicipio :: CadastroSUS -> Municipio -> Quantidade
cidadaosPorMunicipio cadastro munx = length [ municipio | (_,_,_,_,_,municipio,_,_,_) <- cadastro, municipio == munx]

cidadaosPorEstado :: CadastroSUS -> Estado -> Quantidade
cidadaosPorEstado cadastro estx = length [ estado | (_,_,_,_,_,_,estado,_,_) <- cadastro, estado == estx]

cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio-> FaixaIdade -> Data -> Quantidade
cidadaosPorMunicipioIdade cadastro munx faixax datax = length [ municipio | (_,_,_,datanasc,_,municipio,_,_,_) <- cadastro, municipio == munx, checaIdade (idadeAtual datanasc datax) faixax ]

cidadaosPorEstadoIdade :: CadastroSUS -> Estado -> FaixaIdade -> Data -> Quantidade
cidadaosPorEstadoIdade cadastro estx faixax datax = length [estado | (_,_,_,datanasc,_,_,estado,_,_) <- cadastro, estado == estx, checaIdade (idadeAtual datanasc datax) faixax]

--Função auxiliar.

checaIdade :: Int -> FaixaIdade -> Bool
checaIdade idadex faixax
    |  fst faixax <= idadex && idadex <= snd faixax = True
    | otherwise = False

--Funções para gerar listas. 

listaMunicipioFaixas :: CadastroSUS -> Municipio -> Data -> [FaixaIdade] -> IO ()
listaMunicipioFaixas cadastro munx faixax datax = putStrLn
    ("MUNICIPIO:" ++ show munx ++
     "\n\nFAIXAS DE IDADE               QUANTIDADE\n\n" ++
    formataLinhas (geraListaMunicipioFaixas cadastro munx faixax datax)++
    formataTotal (geraListaMunicipioFaixas cadastro munx faixax datax) )

listaEstadoFaixas :: CadastroSUS -> Estado -> Data -> [FaixaIdade] -> IO ()
listaEstadoFaixas cadastro estx faixax datax = putStrLn
    ("ESTADO:" ++ show estx ++
     "\n\nFAIXAS DE IDADE               QUANTIDADE\n\n" ++
    formataLinhas (geraListaEstadoFaixas cadastro estx faixax datax)++
    formataTotal (geraListaEstadoFaixas cadastro estx faixax datax))

--Funções para formatar as listas.

geraListaMunicipioFaixas :: CadastroSUS -> Municipio -> Data -> [FaixaIdade] -> [(FaixaIdade, Quantidade)]
geraListaMunicipioFaixas cadastro munx datax faixalista = zip [faixas | (faixas) <- faixalista] [cidadaosPorMunicipioIdade cadastro munx quantidades datax | quantidades <- faixalista]

geraListaEstadoFaixas :: CadastroSUS -> Estado -> Data-> [FaixaIdade] -> [(FaixaIdade, Quantidade)]
geraListaEstadoFaixas cadastro estx datax faixalista = zip [faixas | (faixas) <- faixalista] [ cidadaosPorEstadoIdade cadastro estx quantidades datax| quantidades <- faixalista]

formataQuant :: Quantidade -> QuantidadeFormatada
formataQuant quantx = concat (replicate (ajuste quantx) " ") ++ show quantx

ajuste :: Quantidade -> Int
ajuste quantx = 35 - length (show quantx)

formataUmaLinha :: (FaixaIdade, Quantidade)-> LinhaFormatada
formataUmaLinha (faixax, quantx) = formataFaixa faixax++formataQuant quantx

formataFaixa :: FaixaIdade -> FaixaFormatada
formataFaixa (inicial, final) = show inicial ++ "-" ++ show final

formataLinhas :: [(FaixaIdade, Quantidade)] -> LinhasFormatadas
formataLinhas linhas = concat [ quebraLinha (formataUmaLinha linha) | linha <- linhas]

quebraLinha :: LinhaFormatada -> String
quebraLinha linha =  linha ++"\n"

formataTotal :: [(FaixaIdade,Quantidade)] -> TotalFormatado
formataTotal lista =  "\nTOTAL" ++ formataQuant(sum [quantidade | (faixa, quantidade) <- lista])

--Banco de dados dos vacinados.

cadastroVac :: Vacinados
cadastroVac = [
    (22233344400,[("Pfizer", (12,07,2021))]),
    (11223344556, [("Astrazeneca", (8,08,2021)),("Astrazeneca", (26,09,2021))]), 
    (88812121212,[("CoronaVac", (30,07,2021))]),
    (99999888887, [("Astrazeneca", (2,08,2021)),("Astrazeneca", (20,09,2021))])
    ]

--Função para aplicar a primeira dose.

aplicaPrimDose :: CPF -> CadastroSUS -> FaixaIdade -> Municipio -> Vacina -> Data -> Vacinados -> Vacinados
aplicaPrimDose cpfx cadastro faixax munx vacinax datax cadastrovac
    | checaPrimDose cpfx cadastrovac == True = error "A primeira dose ja foi aplicada no usuário."
    | checaIdade (idadeAtual (selecionaNasc cpfx cadastro) datax) faixax == False = error "O usuário nao esta na faixa de idade inserida."
    | checaResidencia cpfx cadastro munx == False = error "A vacinação só é permitida para residentes do município informado, atualize o cadastro do usuário."
    | vacinax == "Janssen" = cadastrovac ++ [(cpfx,[(vacinax, datax),(vacinax, datax)])]
    | otherwise = cadastrovac ++ [(cpfx,[(vacinax, datax)])]

--Funções auxiliares.

checaPrimDose :: CPF -> Vacinados -> Bool
checaPrimDose cpfx cadastroVac
    | [cpf | (cpf,[(primeiradose)]) <- cadastroVac, cpf == cpfx] == [] = False
    | otherwise = True

checaResidencia :: CPF -> CadastroSUS -> Endereco -> Bool
checaResidencia cpfx cadastro munx
    | [ municipio | (cpf,_,_,_,_,municipio,_,_,_) <- cadastro, cpf == cpfx, municipio == munx] == [] = False
    | otherwise = True

selecionaNasc :: CPF -> CadastroSUS -> Data
selecionaNasc cpfx cadastro = head [ x | (cpf,_,_,x,_,_,_,_,_) <- cadastro, cpf == cpfx]

--Função para aplicar a segunda dose.

aplicaSegDose :: CPF -> Data -> Vacinados -> Vacinados
aplicaSegDose cpfx datax cadastroVac
    | checaPrimDose cpfx cadastroVac == False = error "O usuário ainda não tomou a primeira dose."
    | checaSegDose cpfx cadastroVac == True = error "O usuário já tomou a segunda dose."
    | ehMaior cpfx datax == False = error "O usuário ainda não pode ser vacinado"
    | otherwise = aplicaasegDose cpfx datax cadastroVac

--Funções auxiliares.

checaSegDose :: CPF -> Vacinados -> Bool
checaSegDose cpfx cadastroVac
    | [segdose | (cpf,[(_,_),(segdose)]) <- cadastroVac, cpf == cpfx] == [] = False
    | otherwise = True

ehMaior :: CPF -> Data -> Bool
ehMaior cpfx datax
    | comparaDatas datax (head[ dataprimeiradose | (cpf, [(vacina, dataprimeiradose)]) <- cadastroVac, cpf == cpfx ]) == datax = True 
    | otherwise = False
    
idxVac :: Vacinados -> [(Int,Vacinado)]
idxVac cadastroVac = zip listaCad cadastroVac
    where listaCad = [1..(length cadastroVac)]

posicaoVac :: CPF -> Vacinados -> Int
posicaoVac cpfx cadastroVac
    | lista == [] = 0
    | otherwise = head lista
    where lista = [ pos | (pos, vacinado) <- idxVac cadastroVac , cpfx == pegaCPFVac vacinado ]

pegaCPFVac :: Vacinado -> CPF
pegaCPFVac (cpfx,_) = cpfx

aplicaasegDose :: CPF -> Data -> Vacinados -> Vacinados
aplicaasegDose cpfx datax cadastroVac = parte1 ++ usuarioatualizado ++ parte2
        where pos = posicaoVac cpfx cadastroVac
              parte1 = take (pos - 1) cadastroVac
              parte2 = drop pos cadastroVac
              usuarioatualizado = [ (cpf,[(primeira,dataprim),(primeira, datax)]) | (cpf,[(primeira,dataprim)]) <- cadastroVac, cpf == cpfx]

comparaDatas :: Data -> Data -> Data
comparaDatas (diax,mesx,anox) (diay,mesy,anoy)
    | anox < anoy = (diay,mesy,anoy)
    | anox > anoy = (diax,mesx,anox)
    | anox == anoy && mesx > mesy = (diax,mesx,anox)
    | anox == anoy && mesx < mesy = (diay,mesy,anoy)
    | anox == anoy && mesx == mesy && diax > diay = (diax,mesx,anox)
    | anox == anoy && mesx == mesy && diax < diay = (diay,mesy,anoy)
    | otherwise = error "As datas informadas são iguais"

--Função para atualizar dados do banco de cadastro de vacinados.  

atualizaVacina:: CPF -> TipoDose -> Vacina -> Vacinados -> Vacinados
atualizaVacina cpfx tipodose vacinax cadastroVac 
    | checaPrimDose cpfx cadastroVac == False = error "O usuário ainda não esta cadastrado no sistema."
    | length [ doses | (cpf,[doses]) <- cadastroVac, cpf == cpfx] > tipodose = error "O usuário ainda não recebeu a dose informada"
    | tipodose == 1 = altera1Dose cpfx vacinax cadastroVac
    | tipodose == 2 = altera2Dose cpfx vacinax cadastroVac

--Funções auxiliares.

altera1Dose :: CPF -> Vacina ->  Vacinados -> Vacinados
altera1Dose cpfx vacinax cadastroVac = parte1  ++  usuarioatualizado  ++  parte2
        where pos = posicaoVac cpfx cadastroVac
              parte1 = take (pos - 1) cadastroVac
              parte2 = drop pos cadastroVac
              usuarioatualizado = [ (cpf,[(vacinax,dataprim)]) | (cpf,doses) <- cadastroVac, (vacina,dataprim) <-doses, cpf == cpfx]

altera2Dose :: CPF -> Vacina ->  Vacinados -> Vacinados
altera2Dose cpfx vacinanova cadastroVac = parte1 ++ usuarioatualizado ++ parte2
        where pos = posicaoVac cpfx cadastroVac
              parte1 = take (pos - 1) cadastroVac
              parte2 = drop pos cadastroVac
              usuarioatualizado = [ (cpf,[(primeira,dataprim),(vacinanova,dataseg)]) | (cpf,[(primeira,dataprim),(segdose,dataseg)]) <- cadastroVac, cpf == cpfx]

--Funções que retornam uma certa quantidade de vacinados.

quantidadeDoseMun :: Vacinados -> TipoDose -> Municipio -> CadastroSUS -> Quantidade
quantidadeDoseMun cadastroVac tipodose munx cadastro
    | tipodose == 1 = length ([ cpf | (cpf,_) <- cadastroVac, checaPrimDose cpf cadastroVac, ehdoMunicipio cpf munx cadastro ] ++ [ cpf | (cpf,_) <- cadastroVac, checaSegDose cpf cadastroVac, ehdoMunicipio cpf munx cadastro ])
    | tipodose == 2 = length [ cpf | (cpf,_) <- cadastroVac, checaSegDose cpf cadastroVac, ehdoMunicipio cpf munx cadastro ] 
    | otherwise = error "Informe um tipo de dose valido (1 ou 2)"

quantidadeDoseEst :: Vacinados -> TipoDose -> Estado -> CadastroSUS -> Quantidade
quantidadeDoseEst cadastroVac tipodose estx cadastro
    | tipodose == 1 = length ([ cpf | (cpf,_) <- cadastroVac, checaPrimDose cpf cadastroVac, ehdoEstado cpf estx cadastro ] ++ [ cpf | (cpf,_) <- cadastroVac, checaSegDose cpf cadastroVac, ehdoEstado cpf estx cadastro ])
    | tipodose == 2 = length [ cpf | (cpf,_) <- cadastroVac, checaSegDose cpf cadastroVac, ehdoEstado cpf estx cadastro ]
    | otherwise = error "Informe um tipo de dose valido (1 ou 2)"

quantidadeMunIdDose :: Vacinados -> Municipio -> FaixaIdade -> TipoDose -> Data -> CadastroSUS -> Quantidade
quantidadeMunIdDose cadastroVac munx faixax tipodose dataatual cadastro
    | tipodose == 1 = length ([ cpf | (cpf,_) <- cadastroVac, checaPrimDose cpf cadastroVac, ehdoMunicipio cpf munx cadastro, checaIdade (idadeAtual (selecionaData cpf cadastro) dataatual) faixax ] ++ [ cpf | (cpf,_) <- cadastroVac, checaSegDose cpf cadastroVac, ehdoMunicipio cpf munx cadastro, checaIdade (idadeAtual (selecionaData cpf cadastro) dataatual) faixax ])
    | tipodose == 2 = length [ cpf | (cpf,_) <- cadastroVac, checaSegDose cpf cadastroVac, ehdoMunicipio cpf munx cadastro, checaIdade (idadeAtual (selecionaData cpf cadastro) dataatual) faixax ]
    | otherwise = error "Informe um tipo de dose valido (1 ou 2)"

quantidadeEstIdDose :: Vacinados -> Estado -> FaixaIdade -> TipoDose -> Data -> CadastroSUS -> Quantidade
quantidadeEstIdDose cadastroVac estx faixax tipodose dataatual cadastro
    | tipodose == 1 = length ([ cpf | (cpf,_) <- cadastroVac, checaPrimDose cpf cadastroVac, ehdoEstado cpf estx cadastro, checaIdade (idadeAtual (selecionaData cpf cadastro) dataatual) faixax ] ++ [ cpf | (cpf,_) <- cadastroVac, checaSegDose cpf cadastroVac, ehdoEstado cpf estx cadastro, checaIdade (idadeAtual (selecionaData cpf cadastro) dataatual) faixax ])
    | tipodose == 2 = length [ cpf | (cpf,_) <- cadastroVac, checaSegDose cpf cadastroVac, ehdoEstado cpf estx cadastro, checaIdade (idadeAtual (selecionaData cpf cadastro) dataatual) faixax ]
    | otherwise = error "Informe um tipo de dose valido (1 ou 2)"

--Funções auxiliares.

ehdoMunicipio :: CPF -> Municipio -> CadastroSUS -> Bool 
ehdoMunicipio cpfx munx cadastro
    | [ (cpf,nome,genero,datanasc,endereco,municipio,estado,numero,email) | (cpf,nome,genero,datanasc,endereco,municipio,estado,numero,email) <- cadastro, municipio == munx, cpf == cpfx] == [] = False 
    | otherwise = True 

ehdoEstado :: CPF -> Estado -> CadastroSUS -> Bool 
ehdoEstado cpfx estx cadastro
    | [ (cpf,nome,genero,datanasc,endereco,municipio,estado,numero,email) | (cpf,nome,genero,datanasc,endereco,municipio,estado,numero,email) <- cadastro, estado == estx, cpf == cpfx] == [] = False
    | otherwise = True

idadeAtual :: DataNasc -> Data -> Int
idadeAtual (dia, mes, ano) (diaHJ, mesAtual, anoAtual)
    | mes == mesAtual && dia <= diaHJ = anoAtual - ano
    | mes == mesAtual && dia > diaHJ = anoPassado - ano
    | mes > mesAtual = anoPassado - ano
    | mes < mesAtual = anoAtual - ano
    where
        anoPassado = anoAtual - 1
        
selecionaData :: CPF -> CadastroSUS -> DataNasc
selecionaData cpfx cadastro = head [ x | (cpf,_,_,x,_,_,_,_,_) <- cadastro, cpf == cpfx]  

--Funções que retornam umaa quantidade de pessoas vacinadas por vacina, estado e cidade. 

quantidadeMunVacDose :: Vacinados -> Municipio -> Vacina -> TipoDose -> CadastroSUS -> Quantidade
quantidadeMunVacDose cadastroVac munx vacinax tipodose cadastro
    | tipodose == 1 = length([ cpf | (cpf,_) <- cadastroVac, checaPrimDose cpf cadastroVac, ehdoMunicipio cpf munx cadastro, comparaVac1 cpf vacinax cadastroVac ] ++ [ cpf | (cpf,_) <- cadastroVac, checaSegDose cpf cadastroVac, ehdoMunicipio cpf munx cadastro, comparaVac2 cpf vacinax cadastroVac ] )
    | tipodose == 2 = length[ cpf | (cpf,_) <- cadastroVac, checaSegDose cpf cadastroVac, ehdoMunicipio cpf munx cadastro, comparaVac2 cpf vacinax cadastroVac ]
    | otherwise = error "Informe um tipo de dose valido (1 ou 2)"

quantidadeEstVacDose :: Vacinados -> Estado -> Vacina -> TipoDose -> CadastroSUS -> Quantidade
quantidadeEstVacDose cadastroVac estx vacinax tipodose cadastro
    | tipodose == 1 = length([ cpf | (cpf,_) <- cadastroVac, checaPrimDose cpf cadastroVac, ehdoEstado cpf estx cadastro, comparaVac1 cpf vacinax cadastroVac ] ++ [ cpf | (cpf,_) <- cadastroVac, checaSegDose cpf cadastroVac, ehdoEstado cpf estx cadastro, comparaVac2 cpf vacinax cadastroVac ] )
    | tipodose == 2 = length[ cpf | (cpf,_) <- cadastroVac, checaSegDose cpf cadastroVac, ehdoEstado cpf estx cadastro, comparaVac2 cpf vacinax cadastroVac ]
    | otherwise = error "Informe um tipo de dose valido (1 ou 2)"

--Funções auxiliares.

comparaVac1 :: CPF -> Vacina -> Vacinados -> Bool 
comparaVac1 cpfx vacinax cadastroVac 
    | [ cpf | (cpf,[(vacina1,(data1))]) <- cadastroVac, vacina1 == vacinax, cpf == cpfx ] == [] = False 
    | otherwise = True 

comparaVac2 :: CPF -> Vacina -> Vacinados -> Bool 
comparaVac2 cpfx vacinax cadastroVac 
    | [ cpf | (cpf,[(vacina1,(data1)), (vacina2,(data2))]) <- cadastroVac, vacina1 == vacinax, vacina2 ==vacinax, cpf == cpfx ] == [] = False 
    | otherwise = True

--Função que retorna uma quantidade de pessoas atrasadas na segunda dose no município/estado inserido.

quantidadeMunAtrasados :: Vacinados -> CadastroSUS -> Data -> Municipio -> Quantidade
quantidadeMunAtrasados cadastroVac cadastro dataatual munx = length [ cpf | (cpf,_) <- cadastroVac, ehdoMunicipio cpf munx cadastro, confAtraso cpf dataatual cadastroVac, checaSegDose cpf cadastroVac == False]

quantidadeEstAtrasados :: Vacinados -> CadastroSUS -> Data -> Estado -> Quantidade
quantidadeEstAtrasados cadastroVac cadastro dataatual estx = length [ cpf | (cpf,_) <- cadastroVac, ehdoEstado cpf estx cadastro, confAtraso cpf dataatual cadastroVac, checaSegDose cpf cadastroVac == False]

--Funções auxiliares.

confAtraso :: CPF -> Data -> Vacinados -> Bool 
confAtraso cpfx datax cadastroVac 
    | [ cpf | (cpf,[(vacina1, data1)]) <- cadastroVac, calcAtraso vacina1 data1 datax ] /= [] = True 
    | otherwise = False 

calcAtraso :: Vacina -> Data -> Data -> Bool 
calcAtraso vacx (diaap,mesap,anoap) (diaatt,mesatt,anoatt)
    | vacx == "CoronaVac" && (diaatt - diaap) > 21 || vacx == "CoronaVac" && mesatt > mesap && (30 - diaap) + diaatt > 21 || vacx == "CoronaVac" && anoatt > anoap = True 
    | vacx == "Pfizer" && (mesatt - mesap) >= 3 && diaatt >= diaap || vacx == "Astrazeneca" && (mesatt - mesap) >= 3 && diaatt >= diaap = True
    | otherwise = False

--Função que retorna a quantidade de cidadãos por gênero e estado.    

cidadaosGenEstado :: CadastroSUS -> Genero -> Estado -> Quantidade
cidadaosGenEstado cadastro genx estx = length [ (cpf,nome,gen,datanasc,end,cidade,est,numero,email) | (cpf,nome,gen,datanasc,end,cidade,est,numero,email) <- cadastro, gen == genx, est == estx ]

--Função que retorna a quantidade de vacinados por gênero.

quantidadeGenVac :: Vacinados -> Genero -> CadastroSUS -> Quantidade
quantidadeGenVac cadastroVac gen cadastro = length([ cpf | (cpf,_) <- cadastroVac, checaPrimDose cpf cadastroVac, checaGen cpf gen cadastro ] ++ [ cpf | (cpf,_) <- cadastroVac, checaSegDose cpf cadastroVac, checaGen cpf gen cadastro] )

--Função auxiliar.
checaGen :: CPF -> Genero -> CadastroSUS -> Bool
checaGen cpfx genx cadastro
    | [ (cpf,nome,genero,datanasc,endereco,municipio,estado,numero,email) | (cpf,nome,genero,datanasc,endereco,municipio,estado,numero,email) <- cadastro, cpf == cpfx, genero == genx] /= [] = True 
    | otherwise = False

--Função que gera lista da quantidade de cidadãos por gênero e estado.

listaCidadaoGenEstado :: CadastroSUS -> Estado -> [Genero] -> IO()
listaCidadaoGenEstado cadastro estx listagenero = putStrLn 
 ("\n\n================ESTADO:" ++ show estx ++ "================\n"++
 formataLinhas1 (geraListaCidadaoGenEstado cadastro estx listagenero ) ++
 "M: Masculino F: Feminino X: Neutro e outros\n==========================================="  ++
 formataTotal1 (geraListaCidadaoGenEstado cadastro estx listagenero )
 
 )

--Funções que formatam a lista da quantidade de cidadãos por gênero e estado.

geraListaCidadaoGenEstado :: CadastroSUS -> Estado -> [Genero] -> [(Genero, Quantidade)]
geraListaCidadaoGenEstado cadastro estx faixalista = zip [faixas | (faixas) <- faixalista] [cidadaosGenEstado cadastro quantidades estx  | quantidades <- faixalista]

formataQuant1 :: Quantidade -> QuantidadeFormatada
formataQuant1 quantx = concat (replicate (ajuste1 quantx) " ") ++ show quantx

ajuste1 :: Quantidade -> Int
ajuste1 quantx = 40 - length (show quantx)

formataUmaLinha1 :: (Genero, Quantidade)-> LinhaFormatada
formataUmaLinha1 (genero, quantx) =  show genero++formataQuant1 quantx

formataLinhas1 :: [(Genero, Quantidade)] -> LinhasFormatadas
formataLinhas1 linhas = concat [ quebraLinha1 (formataUmaLinha1 linha) | linha <- linhas]

quebraLinha1 :: LinhaFormatada -> String
quebraLinha1 linha =  linha ++"\n"

formataTotal1 :: [(Genero,Quantidade)] -> TotalFormatado
formataTotal1 lista =  "\nTotal:" ++ formataQuantTotal(sum [quantidade | (faixa, quantidade) <- lista])

formataQuantTotal :: Quantidade -> QuantidadeFormatada
formataQuantTotal quantx = concat (replicate (ajusteTotal quantx) " ") ++ show quantx

ajusteTotal :: Quantidade -> Int
ajusteTotal quantx = 37 - length (show quantx)