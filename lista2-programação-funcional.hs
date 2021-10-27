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

--Banco de dados 

cadastro :: CadastroSUS
cadastro = [(26716347665, "Paulo Souza", 'M' , (11,10,1996),"Rua A, 202", "Muribeca", "SE", "999997000", "psouza@gmail.com"),   (87717347115, "Ana Reis", 'X' , (5,4,2000), "Rua B, 304", "Aracaju", "SE", "999826004", "areis@gmail.com"),
            (11223344556, "Cesar Oliveira", 'M', (5,1,1990), "Rua X, 18", "Sao Paulo", "SP", "981817070", "ccohen@gmail.com"),  (99999888887, "Arthur Cervero", 'M', (22,4,1992), "Rua X, 19", "Sao Paulo", "SP", "989891010", "acervero@gmail.com"),
            (22233344400, "Elizabeth Webber", 'F', (3,1,1992), "Rua Y, 50", "Campinas", "SP", "999911822", "lizweb@gmail.com"), (88812121212, "Thiago Fritz", 'M', (27,10,1986), "Rua Z, 90", "Aracaju", "SE", "91111999999", "thiagof@gmail.com")]

checaCPF :: CPF -> CadastroSUS -> Bool
checaCPF _ [] = False
checaCPF cpfx ((cpf,_,_,_,_,_,_,_,_):xs)
    | cpfx == cpf = True 
    | otherwise = checaCPF cpfx xs
 