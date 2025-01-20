module Questao6 where
import Data.List
import Test.HUnit

{-|
Esta função recebe uma lista de listas de string unindo essas listas em uma só lista,

de seguida zipa de modo a formar um tuplo cujo o segundo elemento vai do 1 até ao valor

do comprimento da lista concateada

@
f :: [[String]] -> [(String,Int)]
f lq = zip (concat lq) [1..]
@

==__Exemplos de Utilização:__
>>> f [["A", "B", "C"] []]
[("A", 1), ("B", 2), ("C", 3)]

>>> f [["A", "B", "C"] ["D", "E"]]
[("A, 1"), ("B, 2"), ("C, 3"), ("D, 4"), ("E, 5")]
-}

f1 :: [[String]] -> [(String,Int)]
f1 lq = zip (concat lq) [1..]


{-|

@
f2 :: [String] -> Int -> [[(String,Int)]]
f2 [] _ = []
f2 l n = let 
            (l1,l2) = splitAt n l
            l1' = zip l1 [1..]
        in l1' : f2 l2 n
@

==__Exemplos de Utilização:__
>>> f2 ["A", "B", "C", "D", "E"] 2
[[("A", 1), ("B", 2)], [("C", 1), ("D", 2)], [("E", 1)]]
-}


f2 :: [String] -> Int -> [[(String,Int)]]
f2 [] _ = []
f2 l n = let 
            (l1,l2) = splitAt n l
            l1' = zip l1 [1..]
        in l1' : f2 l2 n

{-|
Esta função recebe uma lista de Strings em que é usada uma função auxiliar de modo
a transformar a string à cabeça numa lista de String isolando a head e o last dessa lista
transformando de volta numa string de seguida aplicando a recursiva.
Volta se à função principal em que Transforma essa lista de strings em uma string cujo 
cada elemento da lista anterior está separado por &

@
f3 :: [String] -> String
f3 [] = []
f3 ln = unwords ( intersperse "&" ln’ )
    where 
        ln' = aux ln
        aux :: [String] -> [String]
        aux [] = []
        aux (n:ns) = 
            let 
                lp = words n
                n' = unwords [head lp, last lp]
            in n' : aux ns
@


==__Exemplos de Utilização:__
>>> f3 ["Daniel Marcos Costa", "Custodio Grande Gay", "Andre Pingu Burro"]
"Daniel Costa & Custodio Gay & Andre Burro"

>>> f3 ["Daniel Marcos Costa"]
"Daniel Costa"

-}



f3 :: [String] -> String
f3 [] = []
f3 ln = unwords (intersperse "&" ln')
    where 
        ln' = aux ln
        aux :: [String] -> [String]
        aux [] = []
        aux (n:ns) = 
            let 
                lp = words n
                n' = unwords [head lp, last lp]
            in n' : aux ns




{-|
@
f4 :: String -> String
f4 [] = []
f4 s = unwords (li ++ [last ln]) 
    where 
        ln = words s 
        li = aux (init ln) 
        aux :: [String] -> [String] 
        aux [] = []
        aux (n:ns) = [head n] : aux ns  
@

==__Exemplos de Utilização:__
>>> f4 "Daniel Marcos Costa"
"D M Costa"
>>> f4 "Daniel Costa"
"D Costa"
-}

f4 :: String -> String
f4 [] = []
f4 s = unwords (li ++ [last ln]) 
    where 
        ln = words s 
        li = aux (init ln) 
        aux :: [String] -> [String] 
        aux [] = []
        aux (n:ns) = [head n] : aux ns  

teste1 = "f4 \"Daniel Marcos Costa\"" ~: "D M Costa" ~=? f4 "Daniel Marcos Costa"

{-| 

@
f5 :: [String] -> [(String,String)] 
f5 ls = let 
            n = div (length ls) 2 
            (l1,l2) = splitAt n ls   
        in zip (l1 ++ [" -- "]) l2
@

==__Exemplos de Utilização:__
>>> f5 ["A", "B", "C", "D", "E"]
[("A", "C"), ("B", "D"), ("--", "E")]

>>> f5 ["A", "B", "C", "D"]
[("A", "C"), ("B", "D")]
-}




f5 :: [String] -> [(String,String)] -- ["A", "B", "C", "D", "E"]
f5 ls = let 
            n = div (length ls) 2 -- length = 4 entao n = 2
            (l1,l2) = splitAt n ls   -- (["A", "B"], ["C", "D", "E"])
        in zip (l1 ++ [" -- "]) l2   -- [("A", "C"), ("B", "D"), ("--", "E")]

{-|

Esta função recebe uma String de seguida transforma essa lista em uma lista de Strings,
de seguida cria um tuplo de listas cujo cada comprimento de cada lista é metade do comprimento
da lista original.
Seguidamente une essas duas listas sendo inserido 
no meio a string "##" por fim transforma essa lista fina
em uma String

@
f6 :: String -> String 
f6 t = 
    let 
        lp = words t 
        (lp1,lp2) = splitAt (div (length lp) 2) lp 
    in unwords (lp1 ++ ["##"] ++ lp2) 
@

==__Exemplos de Utilização:__
>>> f6 ""
"##"
>>> f6 "A B C"
"A ## B C"
-}

f6 :: String -> String 
f6 t = 
    let 
        lp = words t -- "Daniel Marcos Costa" -> ["Daniel", "Marcos", "Costa"]
        (lp1,lp2) = splitAt (div (length lp) 2) lp -- (["Daniel"], ["Marcos", "Costa"])
    in unwords (lp1 ++ ["##"] ++ lp2) -- ["Daniel", "##", "Marcos", "Costa"] -> "Daniel ## Marcos Costa"



f7 :: [(String,Int)] -> [String]
f7 l = aux l (maximum l2) -- ["A", "C"]
    where 
        (l1,l2) = unzip l -- (["A", "B", "C"], [4, 3, 4])
        aux :: [(String,Int)] -> Int -> [String] -- [("A", 1), ("B", 2), ("C", 1)] 1 -> ["A", "C"]
        aux [] _ = []
        aux ((s,k):t) n
                | k==n = s : aux t n
                | otherwise = aux t n

f8 :: String -> Char -> Int
f8 [] _ = 0
f8 t c = aux lp c -- 2
    where 
        lp = words t -- "Ola Tudo Bem" -> ["Ola", "Tudo", "Ole"]
        aux :: [String] -> Char -> Int
        aux [] _ = 0
        aux (p:lp) c
            | head p == c = 1 + aux lp c
            | otherwise = aux lp c

f9 :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
f9 l [] = []
f9 l ((z, w):xs)
            | elem (z, w) l = (z, w) : f9 l xs
            | otherwise = f9 l xs


f10 :: [Int] -> [Int]
f10 [] = []
f10 (x:xs) = f10(filtro1 x xs) ++ [x] ++ f10 (filtro2 x xs)
    where
        filtro1 x [] = []
        filtro1 x (y:ys) = if y < x then y : filtro1 x ys else filtro1  x ys
        filtro2 x [] = []
        filtro2 x (y:ys) = if y >= x then y : filtro2 x ys else filtro2 x ys 