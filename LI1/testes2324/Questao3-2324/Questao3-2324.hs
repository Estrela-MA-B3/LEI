{-| 
Module      : Testes34AnoPassado
Description : Testes.
Copyright   : Estrela

Isto é uma descrição
-}
module Testes34AnoPassado where
{-| esta função calcula uma listas com as abreviaturas das Strings exceto a última

@
f1 :: [String] -> [String]
f1 [x] = [x]
f1 (h:t) = [head h] : f1 t
@

== __Exemplos de Utilização:__
>>> f1 ["Ana", "Costa", "Aguiar"]
["A", "C", "Aguiar"]
-}

-- 1
f1 :: [String] -> [String]
f1 [x] = [x]
f1 (h:t) = [head h] : f1 t

-- 2
{-|
type Ponto = (Double, Double)
-}

type Ponto = (Double, Double)

{-| data Figura = Circulo Ponto Double |
              Rectangulo Ponto Ponto |
              Quadrado Ponto Double deriving Show 
-}

data Figura = Circulo Ponto Double |
              Rectangulo Ponto Ponto |
              Quadrado Ponto Double deriving Show

{-| esta função calcula as áreas de um circulo, quadrado ou rectangulo


type Ponto = (Double, Double)
data Figura = Circulo Ponto Double |
              Rectangulo Ponto Ponto |
              Quadrado Ponto Double deriving Show

@
area :: Figura -> Double
area (Circulo (x, y) r) = pi*r^2
area (Quadrado (x, y) l) = l*l
area (Rectangulo (a, b) (c, d)) = (abs (c - a) * abs (d - b))
@

==__Exemplos de Utilização:__
>>> area (Circulo (0, 0) 1)
3.14159
 -}

area :: Figura -> Double
area (Circulo (x, y) r) = pi*r^2
area (Quadrado (x, y) l) = l*l
area (Rectangulo (a, b) (c, d)) = (abs (c - a) * abs (d - b))


{-| esta função calcula a soma das áreas de uma lista de figuras

@
f2 :: [Figura] -> Double
f2 [x] = area x
f2 (h:t) = area h + f2 t
@

==__Exemplos de Utilização:__
>>> f2 [(Quadrado (0, 1) 4), (Rectangulo (1, 2) (4, 6))]
28.0
 -}


f2 :: [Figura] -> Double
f2 [x] = area x
f2 (h:t) = area h + f2 t





-- 3
{-| 
type Aluno = (String, Int)
-}


type Aluno = (String, Int)

{-|  Esta função calcula os alunos em um dado intervalo de notas

@
f3 :: [Aluno] -> (Int, Int) -> [String]
f3 [] _ = []  
f3 ((n, i):t) (x, y) 
    | i >= x && i <= y = n : f3 t (x, y)  
    | otherwise = f3 t (x, y) 
@

==__Exemplos de Utilização:__

>>> f3 [("Daniel", 12), ("Andre", 16), ("Tiago", 18)] (11, 16)
["Daniel","Andre"]
 -}

f3 :: [Aluno] -> (Int, Int) -> [String]
f3 [] _ = []  
f3 ((n, i):t) (x, y) 
    | i >= x && i <= y = n : f3 t (x, y)  
    | otherwise = f3 t (x, y)  



{-type Hora = (Int, Int)

time :: Hora -> Int -> Hora
time (h, m) n =
    let totalMin = m + n              -- Soma os minutos atuais com os minutos a adicionar
        horasAdicionais = totalMin `div` 60  -- Calcula quantas horas adicionais
        minutosRestantes = totalMin `mod` 60 -- Calcula os minutos que sobram
        novaHora = (h + horasAdicionais) `mod` 24 -- Ajusta a hora, garantindo que ela fique no intervalo 0-23
    in (novaHora, minutosRestantes)



f4 :: [Hora] -> Int -> [Hora]
f4 [] _ = []  -- Caso base: se a lista for vazia, retorna lista vazia
f4 ((x, y):t) n = time (x, y) n : f4 t n  -- Aplica a função time e recorre sobre a lista -}


{-|
@
data Hora = H Int Int deriving (Show, Eq)
@
-}

data Hora = H Int Int deriving (Show, Eq)

{-|
@
data Time = AM Int Int | PM Int Int deriving (Show, Eq)
@
-}
data Time = AM Int Int | PM Int Int deriving (Show, Eq)




{-| 
Esta função passa do formato de horas Americano para o formato de horas usado em Portugal

@
f5Aux :: Time -> Hora
f5Aux (AM h m) = (H h m)
f5Aux (PM h m) = (H (h + 12) m)
@

==__Exemplos de Utilização:__
>>> f5Aux (PM 4 20)
H 16 20
>>> f5Aux (AM 4 20)
H 4 20
-}

f5Aux :: Time -> Hora
f5Aux (AM h m) = (H h m)
f5Aux (PM h m) = (H (h + 12) m)


{-| 
Esta função passa do formato de horas Americano para o formato de horas usado em Portugal

@
f5 :: [Time] -> [Hora]
f5 [] = []
f5 (h:t) = f5Aux h : f5 t
@

==__Exemplos de Utilização:__
>>> f5 [(PM 10 20), (AM 7 30), (PM 0 20)]
[H 22 20,H 7 30,H 12 20]

-}


f5 :: [Time] -> [Hora]
f5 [] = []
f5 (h:t) = f5Aux h : f5 t

{-|
@
type Coordenada = (Float, Float)
@
-}

type Coordenada = (Float, Float)

{-|
Esta função verifica se um ponto está contido em uma circunferencia

@
f6Aux :: Coordenada -> Coordenada -> Float -> Bool
f6Aux (x, y) (z, w) r 
        | (sqrt ((z-x)^2 + (w-r)^2)) <= r = True
        | otherwise = False
@

==__Exemplos de Utilização:__
>>> f6Aux (4, 5) (0, 0) 2
False
>>> f6Aux (4, 5) (0, 0) 9
True
-}

f6Aux :: Coordenada -> Coordenada -> Float -> Bool
f6Aux (x, y) (z, w) r 
        | (sqrt ((z-x)^2 + (w-y)^2)) <= r = True
        | otherwise = False


{-|
Esta função é aplicada a saber se um conjunto de pontos estão dentro de uma circunferencia

@
f6 :: [Coordenada] -> Coordenada -> Float -> Bool
f6 [] (x, y) r = True
f6 (h:t) (x, y) r 
            | f6Aux h (x, y) r == True = f6 t (x, y) r
            | otherwise = False
@

==__Exemplos de Utilização:__
>>> f6 [(3, 0), (5, 2), (1, 3)] (0, 0) 3
False
>>> f6 [(1, 1), (2, 2), (3, 3)] (0, 0) 5
True
-}        

f6 :: [Coordenada] -> Coordenada -> Float -> Bool
f6 [] (x, y) r = True
f6 (h:t) (x, y) r 
            | f6Aux h (x, y) r == True = f6 t (x, y) r
            | otherwise = False



{-| Verifica se um ponto está inserido dentro do rectangulo

@
f7Aux :: Coordenada -> Coordenada -> Coordenada -> Bool
f7Aux (a, b) (c, d) (x, y)
        | x >= a && x <= c && y >= b && y <= d = True
        | otherwise = False
@

==__Exemplo de Utilização:__
>>> f7Aux (2, 1) (7, 5) (3, 4)
True
>>> f7Aux (2, 4) (2, 4) (3, 9)
False

-}            


f7Aux :: Coordenada -> Coordenada -> Coordenada -> Bool
f7Aux (a, b) (c, d) (x, y)
        | x >= a && x <= c && y >= b && y <= d = True
        | otherwise = False

{-|
Verifica se a Trajetória representada por um conjunto de pontos de uma pessoa está dentro de um Retangulo

@
f7 :: Coordenada -> Coordenada -> [Coordenada] -> Bool
f7 (a, b) (c, d) [] = True
f7 (a, b) (c, d) (h:t)
        | f7Aux (a, b) (c, d) h == True = f7 (a, b) (c, d) t
        | otherwise = False
@

==__Exemplos de Utilização__
>>> f7 (1, 2) (5, 9) [(1, 2), (3, 4), (4, 5)]
True
>>> f7 (1, 2) (5, 9) [(1, 2), (3, 4), (4, 5), (9, 10)]
False
-}



f7 :: Coordenada -> Coordenada -> [Coordenada] -> Bool
f7 (a, b) (c, d) [] = True
f7 (a, b) (c, d) (h:t)
        | f7Aux (a, b) (c, d) h == True = f7 (a, b) (c, d) t
        | otherwise = False


{-| 
Data type do Movimento

@
data Movimento = N | S | W | E deriving (Show, Eq)
@
-}

data Movimento = N | S | W | E deriving (Show, Eq)


{-|
Função auxiliar do Movimento

@
f8Aux :: Movimento -> Movimento
f8Aux N = S
f8Aux S = N
f8Aux W = E
f8Aux E = W
@

==__Exemplos de Utilização:__
>>> f8Aux N
S
>>> f8Aux S
N
>>> f8Aux W
E
>>> f8Aux E
W
-}

f8Aux :: Movimento -> Movimento
f8Aux N = S
f8Aux S = N
f8Aux W = E
f8Aux E = W


{-|
Aplica dá uma lista dos movimentos opostos à lista inserida

@
f8 :: [Movimento] -> [Movimento]
f8 [] = []
f8 (h:t) = f8Aux h : f8 t
@

==__Exemplo de Utilização:__
>>> f8 [N, S, E, W]
[S, N, W, E]
-}

f8 :: [Movimento] -> [Movimento]
f8 [] = []
f8 (h:t) = f8Aux h : f8 t


{-|
@
type Posicao = (Int, Int)
@
-}

type Posicao = (Int, Int)


{-|
Esta função faz cenas

@
f9 :: [Posicao] -> Posicao
f9 [x] = x
f9 ((x, y):(z, w):t) 
        | y < w = (x, y)
        | otherwise = f9 t
@

==__Exemplos de Utilização:__
>>> f9 [(1, 7), (1, 5), (1, 2), (1, 9)]
(1,2)
>>> f9 [(1, (-1)), (1, 7), (1, 5), (1, 2), (1, 9)]
(1,-1)
-}

f9 :: [Posicao] -> Posicao
f9 [x] = x
f9 ((x, y):(z, w):t) 
        | y < w = (x, y)
        | otherwise = f9 t



