type Aluno = String
type Grupo = (Aluno,Aluno)
type Turno = [Grupo]

-- Função que retorna a lista de alunos, removendo elementos vazios
f :: Turno -> Maybe [Aluno]
f grupos 
    | null grupos || all (== ("-", "-")) grupos = Nothing  -- Se a lista de grupos estiver vazia ou todos são "-"
    | otherwise = let
        alunos = [aluno | (a, b) <- grupos, aluno <- [a, b], aluno /= "-"]  -- Extrai alunos, removendo "-"
      in
        if null alunos then Nothing else Just alunos  -- Retorna Nothing se a lista de alunos estiver vazia, caso contrário Just alunos




f2  :: [Aluno] -> Maybe [Grupo]
f2 [] = Nothing
f2 turno
    | odd (length turno) =   Just (zip l h ++ [(last (init turno), "-")]) 
    | otherwise = Just (zip l h)
        where
            n = div (length turno + 1) 2 
            l = take n turno
            h = drop n turno


f3 :: String -> Maybe String
f3 [] = Nothing
f3 [x] = Nothing
f3 a = Just (unwords fL)
    where
        l = words a
        n = div (length l) 2
        t = take n l
        d = drop n l
        fL = t ++ ["##"] ++ d

f4 :: String -> Maybe String
f4 a 
    | length (x:t) > 1 = Just (unwords (f4Aux (x:t)))
    | otherwise        = Nothing
    where
        (x:t) = words a 
        f4Aux [x] = [x]
        f4Aux (x:t) = [head x] : f4Aux t

f5 :: String -> Char -> Maybe Int
f5 [] c = Nothing
f5 s c 
    |f5Aux c t == 0 = Nothing
    |otherwise = Just (f5Aux c (x:t))
    where
        (x:t) = words s
        f5Aux c [] = 0
        f5Aux c (x:t)
            | head x == c = 1 + f5Aux c t
            | otherwise   = f5Aux c t

-- f6 :: [String] -> Maybe String

f7 :: [String] -> [[(String, Int)]]
f7 [] = [[]]
f7 l
    | length l > 5 = l1' : f7 l2
    | otherwise    = [zip l [1..]]
    where
        (l1, l2) = splitAt 5 l
        l1'      = zip l1 [1..]



f8 :: [[String]] -> [(String, Int)]
f8 [[]] = []
f8 l = zip a [1..]
    where 
        a = concat l


        



    

