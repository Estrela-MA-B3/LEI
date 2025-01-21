module TesteModelo2 where
import Test.HUnit

{-|
A função f pega em uma lista e em um elemento presente na lista, 

zippando a lista original com a lista das posições, criando um par de tuplos

E devolvendo a posição do elemento inserido

@
f :: Eq a => [a] -> a -> Maybe Int
f l x = procura x li
             where li = zip l [0..]
                   procura x [] = Nothing
                   procura x ((y,n):ys) | x==y = Just n
                                        | otherwise = procura x ys

@

==__Exemplos de Utilização:__
>>>  f [1, 2, 3, 4] 3
Just 2
>>> f  [1, 2, 3, 4] 5
Nothing
-}

f :: Eq a => [a] -> a -> Maybe Int
f l x = procura x li
             where li = zip l [0..]
                   procura x [] = Nothing
                   procura x ((y,n):ys) | x==y = Just n
                                        | otherwise = procura x ys


teste1 = "f [1, 2, 3, 4] 3" ~: Just 2 ~=? f [1, 2, 3, 4] 3
teste2 = "f [1, 2, 3, 4] 5" ~: Just 2 ~=? f [1, 2, 3, 4] 5

testes = test [teste1, teste2]


{-|






== Exemplos:

@
g :: Eq a => [[a]] -> [(a, Int)]
g [[]] = []
g [[x]] = [(x, 1)]
g (h:x:t) =  zip (qlqrCena (h:x:t)) [1..]
        where
            qlqrCena [] = []
            qlqrCena (h:x:t) = h ++ x ++ qlqrCena t
@

>>> g [["AA","BB","CC"],["DD","EE","FF","GG"]]
[("AA",1),("BB",2),("CC",3),("DD",4),("EE",5),("FF",6),("GG",7)]
>>> g [["AA","BB","CC"],[]]
[("AA",1),("BB",2),("CC",3)]
-}


g :: Eq a => [[a]] -> [(a, Int)]
g [[]] = []
g [[x]] = [(x, 1)]
g l = zip a [1..]
    where
        a = concat l



 {-g (h:x:t) =  zip (qlqrCena (h:x:t)) [1..]
        where
            qlqrCena [] = []
            qlqrCena (h:x:t) = h ++ x ++ qlqrCena t -}



teste1' = "for g [[\"A\", \"B\", \"C\"], [\"D\", \"E\"]]" ~: [("A",1),("B",2),("C",3),("D",4),("E",5)] ~=? g [["A", "B", "C"], ["D", "E"]]
teste2' = "for g [[\"A\", \"B\", \"C\"], [\"D\", \"E\"]]" ~: [("A",1),("B",2),("C",3),("D",4),("E",5), ("F", 7)] ~=? g [["A", "B", "C"], ["D", "E"]]
teste3' = "for g [[\"A\", \"B\", \"C\"], [\"D\", \"E\"]]" ~: [("A",1),("B",2),("C",3),("D",4)] ~=? g [["A", "B", "C"], ["D", "E"]]

testes' = test [teste1', teste2', teste3']