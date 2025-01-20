module Questao4 where
import Test.HUnit


{- type Aluno = (Nome, Nota)
type Nome = String
type Nota = Int
f1 :: [Aluno] -> [Nome]
f1 [] = []
f1 [(x, y)] = [x]
f1 ((x, y):t) = [x | (x, y), y == maxNota]
        where 
        maxNota = maximum [nota] -}

type UC = [(Aluno, Nota)]
type Aluno = String
type Nota = Int


f2 :: UC -> [Aluno] -> [Nota]
f2 l [] = []
f2 l (h:t) = f2Aux l h : f2 l t

f2Aux :: UC -> Aluno -> Nota
f2Aux ((nome, nota):t) n 
        | nome == n = nota
        | otherwise = f2Aux t n


teste1 = "f2 [(\"D\", 14), (\"A\", 16), (\"B\", 17)] [\"D\", \"B\"]"  ~: [14, 17] ~=? f2 [("D", 14), ("A", 16), ("B", 17)] ["D", "B"]
teste2 = "f2 [(\"D\", 14), (\"A\", 16), (\"B\", 17)] [\"D\", \"A\"]"  ~: [14, 17] ~=? f2 [("D", 14), ("A", 16), ("B", 17)] ["D", "A"]

testes = TestList [teste1, teste2]

type Posicao = (Int, Int)

f3 :: [Posicao] -> [Posicao] -> Bool
f3 [] l = False
f3 (h:t) (x:xs)
        | h == x = True 
        | otherwise = f3 t xs



teste1'' = "f3 [(1, 1), (2, 2), (3, 3)] [(0, 1), (1, 1), (0, 3)]" ~: False ~=? f3 [(1, 1), (2, 2), (3, 3)] [(0, 1), (1, 1), (0, 3)]


f4 :: [Posicao] -> [Posicao] -> [Posicao]
f4 l [] = []
f4 (h:t) (x:xs)
        | h == x   = h : f4 t xs
        | otherwise = f4 t xs



type ListaCompras = [(Produto,Quantidade)]
type Produto = String
type Quantidade = Int
type Preco = Int
type PrecosProdutos = [(Produto, Preco)]

f5 :: ListaCompras -> [Produto] -> ListaCompras
f5 l [] = l
f5 [] l = []
f5 l (x:xs) = f5 (f5Aux l x) xs


f5Aux :: ListaCompras -> Produto -> ListaCompras
f5Aux [] l = []
f5Aux ((x, y):t) p
        | p == x = t
        | otherwise = (x, y) : f5Aux t p


teste1''' = "[(\"A\", 1), (\"B\", 2), (\"C\", 3), (\"D\", 4)] [\"A\", \"C\"]" ~: [("B", 2), ("D", 4)] ~=? f5 [("A", 1), ("B", 2), ("C", 3), ("D", 4)] ["A", "C"]

f6 :: ListaCompras -> PrecosProdutos -> Int
f6 [] l = 0
f6 l [] = 0
f6 (h:t) ((x, y):xs) = f6Aux (h:t) (x, y) + f6 (h:t) xs  


f6Aux :: ListaCompras -> (Produto, Preco) -> Int
f6Aux ((x, y):t) (z, w)
        | z == x = y*w
        | otherwise = f6Aux t (z, w)


testeF6 = "f6 [(\"A\", 1), (\"B\", 3), (\"C\", 2)] [(\"B\", 4), (\"A\", 2)]" ~: 14 ~=? f6 [("A", 1), ("B", 3), ("C", 2)] [("B", 4), ("A", 2)]

f7 :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
f7 l [] = l 
f7 l ((x, y):t) = f7 (f7Aux l (x, y)) t

f7Aux :: [(String, Int)] -> (String, Int) -> [(String, Int)]
f7Aux [] (z, w) = [(z, w)]
f7Aux ((x, y):t) (z, w)
    | x == z && y + w /= 0 = (x, y + w) : t
    | x == z && y + w == 0 = t
    | otherwise = (x, y) : f7Aux t (z, w)


type Nome = String
type Livro = String

f8 :: [(Nome, [Livro])] -> [(Nome, [Livro])] -> [(Nome, [Livro])] 
f8 l [] = l
f8 l ((z, w):xs) = f8 (f8Aux l (z, w)) xs

f8Aux :: [(Nome, [Livro])] -> (Nome, [Livro]) -> [(Nome, [Livro])] 
f8Aux [] (x, l) 
        | null l = []
        | otherwise = [(x, l)]
f8Aux ((x, y):xs) (a, l)
        | x == a && null y && null l = xs
        | x == a    = ((x, y ++ l)) : xs
        | otherwise = (x, y): f8Aux xs (a, l) 



type Stock = [(Produto, Quantidade)]


f9 :: Stock -> [Produto]
f9 [] = [] 
f9 stock = [produto | (produto, qtd) <- stock, qtd == menorQuantidade]
  where
    menorQuantidade = minimum [qtd | (_, qtd) <- stock]


f10 :: Stock -> Stock -> Stock
f10 l [] = l
f10 [] l = []
f10 l ((x, y):t) = f10 (f10Aux l (x, y)) t

f10Aux ::Stock -> (Produto, Quantidade) -> Stock
f10Aux [] l = []
f10Aux ((x, y):t) (z, w)
        | x == z = (x, y + w) : t
        | otherwise =  (x, y) : f10Aux t (z, w)

