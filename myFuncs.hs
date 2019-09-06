-- Soma
soma 1 = 1
soma n = soma(n-1) + n

-- Fatorial

fac 0 = 1
fac n = n * fac(n-1)

-- Fibonaci

fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

-- Switch

mySwitch n | (n == 0) = "Zero"
           | (n == 1) = "Um"
           | otherwise = "Outro"

-- Variáveis anônimas

myAnd :: Bool -> Bool -> Bool
myAnd False _ = False
myAnd _ False = False 
myAnd True True = True

-- Tuplas

func :: (Int,Int) -> (Int,Int) -> (Int,Int)
func (a,b) (c,d) = (a+c,b+d)

-- Manipulando Tuplas
-- fst : extrai o primeiro elemento da tupla
-- snd : extrai o segundo elemento da tupla

nomes :: (String, String, String)
nomes = ("Mateus", "João", "OIOOI")

selec_prim (x,_,_) = x
selec_sec (_,y,_) = y
selec_ter (_,_,z) = z

-- Novos tipos de dados
type Nome = String
type Idade = Int
type Linguagem = String
type Pessoa = (Nome, Idade, Linguagem)

pessoa :: Pessoa
pessoa = ("joao", 20, "haskell")

my_nome :: Pessoa -> Nome
my_nome (n, i, l) = n

-- head 
myHead :: [x] -> x
myHead [] = error "Lista vazia meu chapa!"
myHead (x:_) = x

-- head [] : pega o primeiro elemento
-- tail [] : pega o restante da lista tirando a cabeça
-- [] == [x] : coparar listas
-- x:[y, z] : incrementa valor à lista
-- 1:2:3:[] 

-- Tamanho de uma lista
sizeList [] = 0
sizeList (x:xs) = 1 + sizeList xs

-- Comparar Lista
compList :: [Int] -> [Int] -> Bool
compList [] [] = True
compList [] _ = False
compList _ [] = False
compList (a:b) (c:d) | (a==c) = compList b d
                     | otherwise = False

-- Inverter Lista
invList :: [t] -> [t]
invList [] = []
invList (x:xs) = invList xs ++ [x]

-- Pertence?

pertence :: [Int] -> Int -> Bool
pertence [] _ = False
pertence (x:xs) n | (x == n) = True
                  | otherwise = pertence xs n

-- Maior
maior :: [Int] -> Int
maior [x] = x
maior (x:xs) | (x > maior xs) = x
             | otherwise = maior xs

-- Pares
todos_pares :: [Int] -> Bool
todos_pares [] = True
todos_pares (x:xs) | (mod x 2 == 1) = False
                   | otherwise = todos_pares xs

-- Gerando Listas
-- [x | x <- [1,2,3]]
-- [x | x + 1 <- [1,2,3]]
-- [x | x*x <- [1,2,3]]
-- [x | x <- [1..10]] 
par :: Int -> Bool
par x = mod x 2 == 0
-- [x | x <- [1..10], par x]
-- [x | x <- [1..10], par x, x > 5]
-- [1, 2..10]
-- [(x,y) | x <- [1..5], y <- [6..10]]