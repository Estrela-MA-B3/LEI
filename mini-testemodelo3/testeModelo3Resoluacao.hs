module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Test.HUnit



{-|
Definição de Coordenada

@
type Coordenadas = (Float, Float)
@
-}

type Coordenadas = (Float, Float)

{-|
Definição de Velocidade

@
type Velocidade = (Float, Float)
@
-}

type Velocidade = (Float, Float)

{-|
Definição de Tempo

@
type Tempo = Float
@
-}

type Tempo = Float

{-|
Definição de Estado

@
type Estado = (Coordenadas, Velocidade, Tempo)
@

-}

type Estado = (Coordenadas, Velocidade, Tempo)


{-|
Define o Estado Inicial de uma picture, neste caso a picture em questão começa no ponto (-20, 100),
com um vetor velocidade (0.4, 0.8) e o tempo que o jogo decorre começa em 0 segundos

@
estadoInicial :: Estado
estadoInicial = ((-20, 100), (0.4, -0.8), 0)
@

-}

estadoInicial :: Estado
estadoInicial = ((-20, 100), (0.4, -0.8), 0)

{-|
Função auxiliar utilizada para saber se dois circulos se Intersetam

@
distanciaCirculos :: (Float, Float, Float) -> (Float, Float, Float) -> Bool
distanciaCirculos (x1, y1, r1) (x2, y2, r2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2) <= (r1 + r2)
@

==__Exemplos de Utilização:__

>>> distanciaCirculos (2, 4, 2) (5, 4, 1)
True
>>> (2, 4, 2) (5, 16, 6)
False
-}

distanciaCirculos :: (Float, Float, Float) -> (Float, Float, Float) -> Bool
distanciaCirculos (x1, y1, r1) (x2, y2, r2) =
    sqrt ((x2 - x1)^2 + (y2 - y1)^2) <= (r1 + r2)


{-|

Esta função desenha o estado do programa, utilizando a função auxiliar distanciaCirculo,
em que caso estes circulos se intersetem este é removido do programa até deixarem de se intersetarem

@
desenhaEstado :: Estado -> Picture
desenhaEstado ((x, y), (vx, vy), t)
    | distanciaCirculos (x, y, 20) (199, 199, 30) = Pictures [figura, circulo2, circulo3]
    | distanciaCirculos (x, y, 20) (199, -199, 30) = Pictures [figura, circulo1, circulo3]
    | distanciaCirculos (x, y, 20) (-199, -199, 30) = Pictures [figura, circulo1, circulo2]
    | otherwise = Pictures [figura, circulo1, circulo2, circulo3]
  where
    figura = translate x y $ color c $ circleSolid 20
    c = if vy < 0 then red else green
    circulo1 = translate 199 199 $ color blue $ circleSolid 30
    circulo2 = translate 199 (-199) $ color black $ circleSolid 30
    circulo3 = translate (-199) (-199) $ color rose $ circleSolid 30
@

-}    

desenhaEstado :: Estado -> Picture
desenhaEstado ((x, y), (vx, vy), t)
    | distanciaCirculos (x, y, 20) (199, 199, 30) = Pictures [figura, circulo2]
    | distanciaCirculos (x, y, 20) (199, -199, 30) = Pictures [figura, circulo1]
    | otherwise = Pictures [figura, circulo1, circulo2]
  where
    figura = translate x y $ color c $ circleSolid 20
    c = if vy < 0 then red else green
    circulo1 = translate 199 199 $ color blue $ circleSolid 30
    circulo2 = translate 199 (-199) $ color black $ circleSolid 30

dentroDaJanela :: [(Float, Float, Float)] -> Bool
dentroDaJanela [] = True
dentroDaJanela ((x, y, r):xs) = x + r <= 200 && x - r >= -200 && y + r <= 200 && y - r >= -200 && dentroDaJanela xs

test1 = "Verifica se os circulos estao nas janelas, " ~: True ~=? dentroDaJanela [(-20, 100, 20), (199, 199, 30), (199, (-199), 30)]

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) ((x, y), v, t) = ((x, y + 5), v, t)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) ((x, y), v, t) = ((x, y - 5), v, t)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) ((x, y), v, t) = ((x - 5, y), v, t)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) ((x, y), v, t) = ((x + 5, y), v, t)
reageEvento _ s = s -- ignora qualquer outro evento

reageTempo :: Float -> Estado -> Estado
reageTempo n ((x, y), (vx, vy), t) = ((x + vx', y + vy'), (vx', vy'), t + n)
  where
    vy' = if y + vy <= (-190) || y + vy >= 190 then -vy else vy
    vx' = if x + vx <= (-190) || x + vx >= 190 then -vx else vx

fr :: Int
fr = 50

dm :: Display
dm = InWindow "Jogo Exemplo" -- título da janela
                (400, 400)   -- dimensão da janela
                (200, 200)   -- posição no ecrã

corFundo = greyN 0.5

main :: IO ()
main = play dm -- janela onde irá decorrer o jogo
               corFundo -- cor do fundo da janela
               fr -- frame rate
               estadoInicial -- define estado inicial do jogo
               desenhaEstado -- desenha o estado do jogo
               reageEvento -- reage a um evento
               reageTempo -- reage ao passar do tempo
