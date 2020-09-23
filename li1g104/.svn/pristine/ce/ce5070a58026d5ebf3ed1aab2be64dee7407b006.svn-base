module Main where

import Graphics.Gloss         
import Graphics.Gloss.Data.Picture  
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Environment
import Text.Read
import Data.Maybe
import Data.Char
import Data.List


type Mapa = [String] -- inclui todos os elementos
type Grelha = [String]
type Elementos = [String] -- inclui todo a informaçao menos a grelha
type PUbomba = String
type PUflame = String
type Bomba = String
type Bombas = [Bomba]
type Raio = Int
type Jogador = String
type Linha = Int
type Coluna = Int
type Coordenada = (Coluna,Linha)
type Tempo = Int
type Dimensao = Int
type Direccao = Char


    
-- | Função principal que invoca o jogo.
main :: IO ()
main = do 
          espaçolivre <- loadBMP "espaçolivre.bmp"  -- carrega a imagem dos espaços livres
          parede      <- loadBMP "parede.bmp"         -- carrega a imagem das paredes
          tijolo      <- loadBMP "tijolo.bmp"       -- carrega a imagem dos tijolos
          bomba       <- loadBMP "bomba.bmp"        -- carrega a imagem das bombas
          flame       <- loadBMP "flame.bmp"        -- carrega a imagem das flames
          pubomba     <- loadBMP "pubomba.bmp"      -- carrega a imagem dos PowerUps bomba
          puflame     <- loadBMP "puflame.bmp"      -- carrega a imagem dos PowerUps flame
          joga (((mapa 13 0) ++ ["0 1 1","1 1 11","2 11 1","3 11 11"]),240,[parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame]) desenhaEstado reageEvento reageTempo

-- | Função que cria um jogo.
joga :: Estado -> (Estado -> Picture) -> (Event -> Estado -> Estado) -> (Float -> Estado -> Estado) -> IO ()
joga estadoInicial desenhaEstado reageEvento reageTempo =    
        play dm              -- display mode
             (greyN 0.5)     -- côr do fundo da janela
             fr              -- frame rate
             estadoInicial   -- estado inicial
             desenhaEstado   -- desenha o estado do jogo
             reageEvento     -- reage a um evento
             reageTempo      -- reage ao passar do tempo


-- | Uma representação do estado do jogo.
type Estado = (Mapa,Tempo,[Picture])

-- | O estado inicial do jogo.
estadoInicial :: [Picture] -> Estado
estadoInicial [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] = (((mapa 13 0) ++ ["0 1 1","1 1 11","2 11 1","3 11 11"]),240,[parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame])

{- | Função para adquirir a posicao no mapa de um PowerUP, jogador ou um bomba colocada -}
tupler :: String -> Coordenada
tupler [] = error "erro a criar tuple"
tupler list = (read (takeWhile (/=' ') (drop 1 (dropWhile (/=' ') list)) ) :: Int, read ( takeWhile (/=' ') (drop 1 ( dropWhile (/=' ') (drop 1 (dropWhile (/=' ') list)) ))) :: Int )

{- | Função que utilizamos para transformar a lista de bombas ou flames na lista das suas coordenadas -}
tuplesdados :: [String] -> [Coordenada]
tuplesdados [] = []
tuplesdados (x:xs) = [tupler x] ++ tuplesdados xs

{- | A principal função do nosso mapa, que é a junção de duas funções criadas, uma que gera a grelha e outra que gera a localição dos powerUPs -}
mapa :: Dimensao -> Int -> Mapa
mapa d s 
        | even d || d < 5 = error "dimensao invalida"
        | otherwise = subPUmapa ( subsVmapa (criarMapaV d) (take (contV (criarMapaV d)) $ randomRs (0,99) (mkStdGen s)) )  ++ mapPU (subsVmapa (criarMapaV d) (take (contV (criarMapaV d)) $ randomRs (0,99) (mkStdGen s)))

{- | Função que cria a nossa grelha com 'v's nos quadrados a substituir por valores da lista -}
criarMapaV :: Dimensao -> Mapa
criarMapaV n
  | even n || n < 5 = error "dimensao invalida"
  | otherwise = [[colV n lin col | col <- [1 .. n]] | lin <- [1 .. n]] where
            colV _ 1 _ = '#'
            colV _ _ 1 = '#'
            colV n lin col
                  | lin == n || col == n = '#'
                  | ( lin == 2 || lin == n-1 ) && ( col == 2 || col == 3 || col == n-1 || col == n-2 ) = ' '
                  | ( lin == 2 || lin == n-1 ) && ( col > 3 && col < n-2 ) = 'v'
                  | ( lin == 3 || lin == n-2 ) && ( col == 2 || col == n-1 ) = ' '
                  | ( lin == 3 || lin == n-2 ) && ( col > 2 || col < n-1 ) && odd col  = '#'
                  | ( lin == 3 || lin == n-2 ) && ( col > 3 || col < n-2 ) && even col = 'v'
                  | lin > 3 && lin < n-2 && even lin = 'v'
                  | lin > 3 && lin < n-2 && odd lin && even col = 'v'
                  | lin > 3 && lin < n-2 && odd lin && odd col = '#'

{- | Função que conta o total de 'v's no nosso mapa, ou seja os valores necessários na lista de inteiros gerada inicialmente -}
contV :: [[Char]] -> Int
contV grelha = length (filter (== 'v') (unlines grelha))

{- | Funçao que converte de Int para um dos caracteres aceites no nosso mapa -}
convtoMap :: Int -> Char
convtoMap x 
            | x == 0 || x == 1 = '+'
            | x == 2 || x == 3 = '!'
            | x> 3 && x<40 = '?'
            | (x>39) && (x<100) = ' '
            | otherwise = error "Inteiro invalido na lista"

{- | Funçao que substitui os 'v's do nosso mapa por ‘?’ para representar tijolo, ‘ ‘ para representar uma célula vazia, '+' para representar uma bomba e '!' para representar flames -}
subsVmapa :: Grelha -> [Int] -> Grelha
subsVmapa [] _ = []
subsVmapa list [] = list
subsVmapa mapa list = subsVlin (head mapa) list : subsVmapa (tail mapa) (drop (contV ([head mapa])) list) where
                 subsVlin [] _ = []
                 subsVlin list [] = list
                 subsVlin (x:xs) (y:ys) = if ( x == 'v' ) then [convtoMap y] ++ subsVlin xs ys else [x] ++ subsVlin xs (y:ys)

{- | Funçao que calcula as coordenadas dos powerUps-}
mapPU :: [String] -> [String]
mapPU [] = []
mapPU mapa = mapPUB mapa 0 0 ++ mapPUF mapa 0 0 where
        mapPUB [] _ _ = []
        mapPUB (x:xs) c l = if (elem '+' x) then mapPUBl x 0 l ++ mapPUB xs 0 (l+1) else mapPUB xs 0 (l+1) where
            mapPUBl [] _ _ = []
            mapPUBl (x:xs) c l = if ( x == '+' ) then ( "+" ++ " " ++ (show c) ++ " " ++ (show l) ) : mapPUBl xs (c+1) l else mapPUBl xs (c+1) l
        mapPUF [] _ _ = []
        mapPUF (x:xs) c l = if (elem '!' x) then mapPUFl x 0 l ++ mapPUF xs 0 (l+1) else mapPUF xs 0 (l+1) where
            mapPUFl [] _ _ = []
            mapPUFl (x:xs) c l = if ( x == '!' ) then ( "!" ++ " " ++ (show c) ++ " " ++ (show l) ) : mapPUFl xs (c+1) l else mapPUFl xs (c+1) l

{- |  Funçao que pega no mapa com '+' e '!' a representar os powerUPs e os esconde atrás de tijolos ('?') -}
subPUmapa :: [String] -> [String]
subPUmapa [] = []
subPUmapa (x:xs) = if ( ('+' `elem` x) || ('!' `elem` x) ) then [subPUmapaAux x] ++ subPUmapa xs else [x] ++ subPUmapa xs where
        subPUmapaAux [] = []
        subPUmapaAux (x:xs) = if ( (x == '+') || (x == '!') ) then "?" ++ subPUmapaAux xs else [x] ++ subPUmapaAux xs

-- | Função que desenha o jogo.
desenhaEstado :: Estado -> Picture
desenhaEstado (mapaa,t,[parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame]) 
    | (length (jogadores mapaa) == 1) = selecionaimagem [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (take 1 (head (jogadores mapaa))) 0 0
    | (length (jogadores mapaa) > 1) = Pictures ( [tabuleiro] ++ (desenharespacoslivres [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (take 13 mapaa) (-300) 300 13) ++ (desenharelementos [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (drop 13 mapaa) (tuplesdados (drop 13 mapaa))) ++ (desenhargrelha [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (take 13 mapaa) (-300) 300 13) )
        where
            tabuleiro = Color white (polygon (rectanglePath (650) (650)))
         
desenhargrelha :: [Picture] -> Grelha -> Float -> Float -> Float -> [Picture]
desenhargrelha _ [] _ _ _ = []
desenhargrelha [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (x:xs) c l d = (desenharlinha [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] x c l d) ++ (desenhargrelha [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] xs c (l-50) d)

desenharespacoslivres :: [Picture] -> Grelha -> Float -> Float -> Float -> [Picture]
desenharespacoslivres _ [] _ _ _ = []
desenharespacoslivres [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (x:xs) c l d = (desenharespacoslivreslinha [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] x c l d) ++ (desenharespacoslivres [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] xs c (l-50) d)

desenharespacoslivreslinha :: [Picture] -> String -> Float -> Float -> Float -> [Picture]
desenharespacoslivreslinha [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] linha c l d =
                                     if (d>1)
                                        then  if ((take 1 linha)==" ") then (selecionaimagem [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (take 1 linha) c l) : (desenharespacoslivreslinha [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (drop 1 linha) (c+50) l (d-1))
                                        else (desenharespacoslivreslinha [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (drop 1 linha) (c+50) l (d-1))
                                     else [(selecionaimagem [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (take 1 linha) c l)]

desenharlinha :: [Picture] -> String -> Float -> Float -> Float -> [Picture]
desenharlinha [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] linha c l d = 
                                     if (d>1) 
                                        then  if (((take 1 linha)=="#") || ((take 1 linha)=="?") || (take 1 linha == "f")) then (selecionaimagem [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (take 1 linha) c l) : (desenharlinha [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (drop 1 linha) (c+50) l (d-1))
                                              else (desenharlinha [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (drop 1 linha) (c+50) l (d-1))
                                        else [(selecionaimagem [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (take 1 linha)) c l]

desenharelementos :: [Picture] -> Elementos -> [Coordenada] -> [Picture]
desenharelementos [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (x:xs) ((c,l):ys) = if ((length xs)>0) then ((selecionaimagem [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (take 1 x)) (-300+((fromIntegral c)*50)) (300-((fromIntegral l)*50))) : (desenharelementos [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] xs ys)
                                               else [(selecionaimagem [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] (take 1 x) (fromIntegral ((-300)+(c*50))) (fromIntegral ((300)-(l*50)))) ]

selecionaimagem :: [Picture] -> String -> Float -> Float -> Picture
selecionaimagem [parede,espaçolivre,tijolo,pubomba,puflame,bomba,flame] x c l
               | (x == "#") = Translate c l parede -- Parede
               | (x == " ") = Translate c l espaçolivre  -- espaço livre
               | (x == "?") = Translate c l tijolo  -- tijolo
               | (x == "0") = Translate c l $ (Color green (circle 25)) -- jogador 1
               | (x == "1") = Translate c l $ (Color red (circle 21)) -- jogador 2
               | (x == "2") = Translate c l $ (Color blue (circle 18)) -- jogador 3
               | (x == "3") = Translate c l $ (Color black (circle 15)) -- jogador 4
               | (x == "f") = Translate c l flame   -- flame
               | (x == "+") = Translate c l pubomba -- PowerUp bomba
               | (x == "!") = Translate c l puflame
               | (x == "*") = Translate c l bomba

-- | Função que altera o estado do jogo quando acontece um evento.
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) (mapa,t,imagens) = ((move mapa 0 'U'),t,imagens)
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) (mapa,t,imagens) = ((move mapa 0 'D'),t,imagens)
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (mapa,t,imagens) = ((move mapa 0 'L'),t,imagens)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (mapa,t,imagens) = ((move mapa 0 'R'),t,imagens)
reageEvento (EventKey (SpecialKey KeyF1)    Down _ _) (mapa,t,imagens) = ((move mapa 0 'B'),t,imagens)
reageEvento _ mapa = mapa -- ignora qualquer outro evento


move :: Mapa -> Int -> Char -> Mapa
move mapa j c
    | ( mapa == [] ) = error "erro no mapa"
    | ( j < 0 ) || ( j > 3 ) = error "numero de jogador invalido"
    | ( c == 'B' ) = if (verposibomba (jogador (intToDigit j) mapa) (bombacol mapa)) && ((verjogcolbom j (bombacol mapa) ) < (1 + (length (filter (=='+') (jogador (intToDigit j) (jogadores mapa))))))
                        then grelha mapa ++ bombas mapa ++ flames mapa ++ inserirbomba (criarbomba (tupler (jogador (intToDigit j) mapa)) j (1 + (length (filter (=='!') (jogador (intToDigit j) mapa)))) 10) (bombacol mapa) ++ (jogadores mapa)
                        else mapa
    | ( c == 'D' ) && ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (0,1))=='f') = grelha mapa ++ (bombas mapa) ++ (flames mapa) ++ bombacol mapa ++ (removerjogadormovido j (jogadores mapa))
    | ( c == 'D' ) && ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (0,1))/='f') =
                     if ( ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (0,1))/='#') && ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (0,1))/='?') )
                        then ( if((verPU (moverJogador (jogador (intToDigit j) mapa) (0,1)) ((bombas mapa) ++ (flames mapa)))==False) 
                                  then grelha mapa ++ bombas mapa ++ flames mapa ++ bombacol mapa ++ insert (moverJogador (jogador (intToDigit j) mapa) (0,1)) (removerjogadormovido j (jogadores mapa))
                                  else grelha mapa ++ remPU (moverJogador (jogador (intToDigit j) mapa) (0,1)) ((bombas mapa) ++ (flames mapa)) ++ bombacol mapa ++ insert (addPU (moverJogador (jogador (intToDigit j) mapa) (0,1)) ((bombas mapa) ++ (flames mapa))) (removerjogadormovido j (jogadores mapa)) )                     
                        else mapa
    | ( c == 'U' ) && ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (0,-1))=='f') = grelha mapa ++ (bombas mapa) ++ (flames mapa) ++ bombacol mapa ++ (removerjogadormovido j (jogadores mapa)) 
    | ( c == 'U' ) && ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (0,-1))/='f') =
                     if ( ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (0,-1))/='#') && ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (0,-1))/='?') )
                        then ( if((verPU (moverJogador (jogador (intToDigit j) mapa) (0,-1)) ((bombas mapa) ++ (flames mapa)))==False) 
                                  then grelha mapa ++ bombas mapa ++ flames mapa ++ bombacol mapa ++ insert (moverJogador (jogador (intToDigit j) mapa) (0,-1)) (removerjogadormovido j (jogadores mapa))
                                  else grelha mapa ++ remPU (moverJogador (jogador (intToDigit j) mapa) (0,-1)) ((bombas mapa) ++ (flames mapa)) ++ bombacol mapa ++ insert (addPU (moverJogador (jogador (intToDigit j) mapa) (0,-1)) ((bombas mapa) ++ (flames mapa))) (removerjogadormovido j (jogadores mapa)) )
                        else mapa
    | ( c == 'L' ) && ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) ((-1),0))=='f') = grelha mapa ++ (bombas mapa) ++ (flames mapa) ++ bombacol mapa ++ (removerjogadormovido j (jogadores mapa)) 
    | ( c == 'L' ) && ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) ((-1),0))/='f') =
                     if ( ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) ((-1),0))/='#') && ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) ((-1),0))/='?') )
                        then( if((verPU (moverJogador (jogador (intToDigit j) mapa) ((-1),0)) ((bombas mapa) ++ (flames mapa)))==False) 
                                  then grelha mapa ++ bombas mapa ++ flames mapa ++ bombacol mapa ++ insert (moverJogador (jogador (intToDigit j) mapa) ((-1),0)) (removerjogadormovido j (jogadores mapa))
                                  else grelha mapa ++ remPU (moverJogador (jogador (intToDigit j) mapa) ((-1),0)) ((bombas mapa) ++ (flames mapa)) ++ bombacol mapa ++ insert (addPU (moverJogador (jogador (intToDigit j) mapa) ((-1),0)) ((bombas mapa) ++ (flames mapa))) (removerjogadormovido j (jogadores mapa)) )
                        else mapa
    | ( c == 'R' ) && ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (1,0))=='f') = grelha mapa ++ (bombas mapa) ++ (flames mapa) ++ bombacol mapa ++ (removerjogadormovido j (jogadores mapa)) 
    | ( c == 'R' ) && ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (1,0))/='f') = 
                     if ( ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (1,0))/='#') && ((verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (1,0))/='?') )
                        then( if((verPU (moverJogador (jogador (intToDigit j) mapa) (1,0)) ((bombas mapa) ++ (flames mapa)))==False) 
                                  then grelha mapa ++ bombas mapa ++ flames mapa ++ bombacol mapa ++ insert (moverJogador (jogador (intToDigit j) mapa) (1,0)) (removerjogadormovido j (jogadores mapa))
                                  else grelha mapa ++ remPU (moverJogador (jogador (intToDigit j) mapa) (1,0)) ((bombas mapa) ++ (flames mapa)) ++ bombacol mapa ++ insert (addPU (moverJogador (jogador (intToDigit j) mapa) (1,0)) ((bombas mapa) ++ (flames mapa))) (removerjogadormovido j (jogadores mapa)) )
                        else mapa
    | ( c /= 'U' ) || ( c /= 'D' ) || ( c /= 'L' ) || ( c /= 'R' ) || ( c /= 'B' ) = error "comando invalido"

{- | Função que recebe o input e devolve apenas a grelha de jogo -}
grelha :: [String] -> [String]
grelha [] = []
grelha (x:xs) = if ( head x == '#' ) then [x] ++ grelha xs else []


{- | Função que recebe o input e devolve a lista de bombas -}
bombas :: [String] -> [String]
bombas [] = []
bombas (x:xs) = if ( head x == '+' ) then [x] ++ bombas xs else bombas xs


{- | Função que recebe o input e devolve a lista de flames -}
flames :: [String] -> [String]
flames [] = []
flames (x:xs) = if ( head x == '!' ) then [x] ++ flames xs else flames xs


{- | Função que recebe o input e devolve a lista das bombas colocadas -}
bombacol :: [String] -> [String]
bombacol [] = []
bombacol (x:xs) = if (head x == '*') then [x] ++ bombacol xs else bombacol xs

{- | Função que recebe o input e devolve a lista jogadores -}
jogadores :: [String] -> [String]
jogadores [] = []
jogadores (x:xs) = if ( (head x == '0') || (head x == '1') || (head x == '2') || (head x == '3') ) then [x] ++ jogadores xs else jogadores xs

{- | Função que pega na lista de jogadores e devolve apenas um jogador -}
jogador :: Char -> [String] -> String
jogador _ [] = "jogador invalido"
jogador x (y:ys) = if ( x == head y ) then y else jogador x ys

{- | Função que devolve a ocupação duma determinada posição da grelha -}
verposigrelha :: Grelha -> Coordenada -> Coordenada -> Char
verposigrelha (x:xs) (c,l) (a,b) = verposigrelhaCol (x:xs) ((c+a),(l+b)) where
                    verposigrelhaCol list (c,l) = verposigrelhaLin (list !! l) c where
                        verposigrelhaLin string c = string !! c

{- | Função que verifica se existe alguma bomba na posicao do jogador -}
verposibomba :: String -> [String] -> Bool
verposibomba jogador [] = True
verposibomba jogador (x:xs) = if ( tupler jogador == tupler x ) then False else verposibomba jogador xs

{- | Função que conta quantas bombas um jogador já tem colocadas no mapa -}
verjogcolbom :: Int -> [String] -> Int
verjogcolbom j [] = 0
verjogcolbom j (x:xs) = if ( j == (read (takeWhile (/=' ') (drop 1 (dropWhile (/=' ') (drop 1 (dropWhile (/=' ') (drop 2 x)))))) :: Int)) then 1 + (verjogcolbom j xs) else 0 + (verjogcolbom j xs)

{- | Função que cria a nova string de uma bomba acabada de colocar -}
criarbomba :: (Int,Int) -> Int -> Int -> Int -> String
criarbomba (p,p2) j ra t = "* " ++ show p ++ " " ++ show p2 ++ " " ++ show j ++ " " ++ show ra ++ " " ++ show t

{- | Função que insere a string de uma bomba acabada de colocar na nossa lista de bombas -}
inserirbomba :: String -> [String] -> [String]
inserirbomba nova [] = [nova]
inserirbomba nova (x:xs) = if ((maiortuple (tupler nova) (tupler x))==tupler x) then [nova] ++ (x:xs) else [x] ++ inserirbomba nova xs

{- | Função que copara dois tuples do tipo (coluna,linha) e devolve o maior -}
maiortuple :: (Int,Int) -> (Int,Int) -> (Int,Int)
maiortuple (a,b) (x,y)
                   | (y>b) = (x,y)
                   | (y<b) = (a,b)
                   | (y==b) && (a<x) = (x,y)
                   | (y==b) && (a>x) = (a,b)
                   | (y==b) && (a==x) = (a,b)

{- | Função que verifica se na posição destino do jogador existe algum PU -}
verPU :: String -> [String] -> Bool
verPU _ [] = False
verPU jogador (x:xs) = if ((head x)=='+') || ((head x)=='!') then if (tupler (jogador) == tupler x) then True else verPU jogador xs
                                                                    else verPU jogador xs

{- | Função que adiciona power up a um jogador -}
addPU :: String -> [String] -> String
addPU jogador [] = error "erro a adiciona PU a um jogador"
addPU jogador (x:xs) = if (('+' `elem` jogador) || ('!' `elem` jogador)) && (tupler jogador == tupler x) then (takeWhile (/='!') jogador) ++ (take 1 x) ++ (dropWhile (/='!') jogador) 
                            else if (tupler jogador == tupler x) then jogador ++ " " ++ (take 1 x) else addPU jogador xs 

{- | Função que remove power up dada a um jogador -}
remPU :: String -> [String] -> [String]
remPU _ [] = []
remPU jogador (x:xs) = if (tupler jogador == tupler x) then xs else [x] ++ remPU jogador xs

{- | Função que move o jogadaor na direçao do comando -}
moverJogador :: String -> (Int,Int) -> String
moverJogador jogador (a,b) = takeWhile (/=' ') jogador ++ " " ++ moverJogadorAux (tupler jogador) (a,b) ++ ( dropWhile (/=' ') ( drop 1 ( dropWhile (/=' ') (drop 1 ( (dropWhile (/=' ') jogador)))))) where
            moverJogadorAux (l,c) (b,a)
                    | (a==1) = toString (l,(c+1))
                    | (a==(-1)) = toString (l,(c-1))
                    | (b==1) = toString ((l+1),c)
                    | (b==(-1)) = toString ((l-1),c) where
                        toString (a,b) = show a ++ " " ++ show b

{- | Função que remove o jogador movido da lista de jogadores inicial -}
removerjogadormovido :: Int -> [String] -> [String]
removerjogadormovido j (x:xs) 
                       | ((intToDigit j) == head x) = xs
                       | otherwise = [x] ++ removerjogadormovido j xs




-- | Função que altera o estado do jogo quando o tempo avança @n@ segundos.
reageTempo :: Float -> Estado -> Estado
reageTempo f (s,i,imagens) = (reageBot3 (reageBot2 (reageBot ((avanca s i),(i-1),imagens))))

avanca :: [String] -> Int -> [String]
avanca mapa t = if (t< ((length (head mapa))-2)^2 )
                  then  ( expbombas (bombasaexp (bombacol mapa)) (fecharmapa (((length (head mapa))^2)-t) ((removerflames (grelha mapa)) ++ (bombas mapa) ++ (flames mapa) ++ (decbombas (bombacol mapa)) ++ (jogadores mapa))))
                  else expbombas (bombasaexp (bombacol mapa)) ((removerflames (grelha mapa)) ++ (bombas mapa) ++ (flames mapa) ++ (decbombas (bombacol mapa)) ++ (jogadores mapa))

{- | Função que seleciona as bombas que irão explodir no instante de tempo actual -}
bombasaexp :: [Bomba] -> [Bomba]
bombasaexp [] = []
bombasaexp (x:xs) = if ( (read (drop 1 (dropWhile (/=' ') (drop 3 (dropWhile (/=' ') (drop 1 ( dropWhile (/=' ') (drop 2 x))))))) :: Int ) == 1) then x : bombasaexp xs else bombasaexp xs

{- | Função que remove bombas que irão explodir neste instante do estado actual e reduz 1 instante ao tempo em falta para estas explodirem -}
decbombas :: [Bomba] -> [Bomba]
decbombas [] = []
decbombas (x:xs) = if ( (read (drop 1 (dropWhile (/=' ') (drop 3 (dropWhile (/=' ') (drop 1 ( dropWhile (/=' ') (drop 2 x))))))) :: Int ) == 1) then decbombas xs else ( (reverse (dropWhile (/=' ') (reverse x))) ++ (show (( read (reverse (takeWhile (/=' ') (reverse x))) :: Int) -1))) : (decbombas xs)

{- | Função que explode as bombas -}
expbombas :: [Bomba] -> Mapa -> Mapa
expbombas [] mapa = mapa
expbombas (x:xs) mapa = expbombas xs (expbomba x mapa)

{- | Função auxiliar de expbombas que explode cada bomba -}
expbomba :: Bomba -> Mapa -> Mapa
expbomba x mapa = 
    explodir (-1,0) (tupler x) ( read (reverse (takeWhile (/=' ') (drop 1 (dropWhile (/=' ') (reverse x))))) :: Int) (
        explodir (1,0) (tupler x) ( read (reverse (takeWhile (/=' ') (drop 1 (dropWhile (/=' ') (reverse x))))) :: Int) (
            explodir (0,1) (tupler x) ( read (reverse (takeWhile (/=' ') (drop 1 (dropWhile (/=' ') (reverse x))))) :: Int) ( 
                explodir (0,-1) (tupler x) ( read (reverse (takeWhile (/=' ') (drop 1 (dropWhile (/=' ') (reverse x))))) :: Int) 
                ((inserirflame (tupler x) (grelha mapa)) ++ (bombas mapa) ++ (flames mapa) ++ (bombacol mapa) ++ (removerjogpos (tupler x) (jogadores mapa))) )))

{- | Função que remove jogador que esteja na mesma posição que uma bomba a explodir -}
removerjogpos :: Coordenada -> [Jogador] -> [Jogador]
removerjogpos _ [] = []
removerjogpos p (x:xs) = if (p == (tupler x)) then removerjogpos p xs else [x] ++ (removerjogpos p xs)

{- | Função que produz o efeito das chamas ao explodir uma bomba para um direcção -}
explodir :: Coordenada -> Coordenada -> Raio -> Mapa -> Mapa
explodir (a,b) (c,l) ra mapa = if (ra > 0) 
        then   (if ((verposigrelha (grelha mapa) (c,l) (a,b))=='#') then mapa 
                    else (if ((verposigrelha (grelha mapa) (c,l) (a,b))=='?') then (removertijolo (c+a,l+b) mapa)
                        else (if ((a+c,b+l) `elem` tuplesdados (bombas mapa)) then mapa
                            else (if ((a+c,b+l) `elem` tuplesdados (flames mapa)) then mapa 
                                else  explodir (a,b) (c+a,l+b) (ra-1) ((inserirflame ((a+c),(b+l)) (grelha mapa)) ++ (bombas mapa) ++ (flames mapa) ++ (tempobombaum (a+c,b+l) (bombacol mapa)) ++ (eliminarjogatingidos (a+c,b+l) (jogadores mapa)))   )))) 
        else mapa

-- | função que insere a representação de uma flame na grelha
inserirflame :: Coordenada -> Grelha -> Grelha
inserirflame (c,l) (x:xs) = if (l>0) then [x] ++ (inserirflame (c,(l-1)) xs) else [inserirflamelinha c x] ++ xs where
        inserirflamelinha c (l:ls) = if (c>0) then l : (inserirflamelinha (c-1) ls) else 'f' : ls

-- | Função que remove a representação de flames do instante anterior da grelha
removerflames :: Grelha -> Grelha
removerflames [] = []
removerflames (x:xs) = if ('f' `elem` x) then [removerflameslinha x] ++ (removerflames xs) else [x] ++ (removerflames xs) where
        removerflameslinha [] = []
        removerflameslinha (y:ys) = if (y=='f') then (' ') : (removerflameslinha ys) else y : (removerflameslinha ys)

{- | Função que remove um tijolo do mapa -}
removertijolo :: Coordenada -> Mapa -> Mapa
removertijolo (c,l) (x:xs) = if (l>0) then [x] ++ (removertijolo (c,l-1) xs) else [(removertijoloAux c x)] ++ xs where
    removertijoloAux c x = if (c>0) then (take 1 x) ++ (removertijoloAux (c-1) (drop 1 x)) else " " ++ (drop 1 x)

{- | Função que passa o tempo que falta para uma bomba explodir directamente para 1 -}
tempobombaum :: Coordenada -> [Bomba] -> [Bomba]
tempobombaum _ [] = []
tempobombaum a (x:xs) = if ((tupler x) == a) then ((reverse (dropWhile (/=' ') (reverse x))) ++ "1") : xs else [x] ++ tempobombaum a xs

{- | Função que elimina os jogadores que estão numa determinada posição -}
eliminarjogatingidos :: Coordenada -> [Jogador] -> [Jogador]
eliminarjogatingidos _ [] = []
eliminarjogatingidos a (x:xs) = if ((tupler x) == a) then eliminarjogatingidos a xs else [x] ++ (eliminarjogatingidos a xs)

{-| Função que coloca uma parede numa posição, eliminando todos os seus elementos, com base no tempo de forma espiral -}
fecharmapa :: Int -> Mapa -> Mapa
fecharmapa t mapa = colocarparede (posicaoafechar 'R' (length (head mapa)) 0 0 t) (grelha mapa) ++ (removertudopos (posicaoafechar 'R' (length (head mapa)) 0 0 t) ((bombas mapa) ++ (flames mapa) ++ (bombacol mapa) ++ (jogadores mapa)))

{- | Função que remove os elementos que estão numa determinada posição -}
removertudopos :: Coordenada -> Elementos -> Elementos
removertudopos _ [] = []
removertudopos p (x:xs) = if (p==(tupler x)) then removertudopos p xs else x : (removertudopos p xs)

{- | Função que, na grelha de jogo, coloca uma parede numa dada posição-}
colocarparede :: Coordenada -> Grelha -> Grelha
colocarparede (c,l) grelha = if (l>0) then head grelha : colocarparede (c,l-1) (drop 1 grelha)
                                          else [(colocarparedeAux c (head grelha))] ++ (drop 1 grelha) where
    colocarparedeAux c linha = if (c==0) then '#' : (drop 1 linha)
                                             else (head linha) : (colocarparedeAux (c-1) (drop 1 linha))

{- | Função que, dada a grelha de jogo e o tempo, indica qual posição se deve fechar nesse instante -}
posicaoafechar :: Char -> Dimensao -> Coluna -> Linha -> Tempo -> Coordenada
posicaoafechar 'R' n c l t = if (t>n) then posicaoafechar 'D' (n-1) (c+n-1) (l+1) (t-n) 
                                           else (if (t==1) then (c,l) else posicaoafechar 'R' n (c+1) l (t-1) )
posicaoafechar 'D' n c l t = if (t>n) then posicaoafechar 'L' n (c-1) (l+n-1) (t-n) 
                                           else (if (t==1) then (c,l) else posicaoafechar 'D' n c (l+1) (t-1) )
posicaoafechar 'L' n c l t = if (t>n) then posicaoafechar 'U' (n-1) (c-n+1) (l-1) (t-n)
                                           else (if (t==1) then (c,l) else posicaoafechar 'L' n (c-1) l (t-1) )
posicaoafechar 'U' n c l t = if (t>n) then posicaoafechar 'R' n (c+1) (l-n+1) (t-n)
                                           else (if (t==1) then (c,l) else posicaoafechar 'U' n c (l-1) (t-1) )

-- | Função que recebe um estado e devolve um novo estado com os movimentos de cada Bot.
reageBot :: Estado -> Estado
reageBot (mapa,tempo,imagens)
        | (jogador '1' mapa == "jogador invalido") = (mapa,tempo,imagens)
        | ((bot mapa 1 tempo) == Just 'U') = ((move mapa 1 'U'),tempo,imagens)
        | ((bot mapa 1 tempo) == Just 'D') = ((move mapa 1 'D'),tempo,imagens)
        | ((bot mapa 1 tempo) == Just 'L') = ((move mapa 1 'L'),tempo,imagens)
        | ((bot mapa 1 tempo) == Just 'R') = ((move mapa 1 'R'),tempo,imagens)
        | ((bot mapa 1 tempo) == Just 'B') = ((move mapa 1 'B'),tempo,imagens)
        | otherwise = (mapa,tempo,imagens) 

reageBot2 :: Estado -> Estado
reageBot2 (mapa,tempo,imagens)
        | (jogador '2' mapa == "jogador invalido") = (mapa,tempo,imagens)
        | ((bot mapa 2 tempo) == Just 'U') = ((move mapa 2 'U'),tempo,imagens)
        | ((bot mapa 2 tempo) == Just 'D') = ((move mapa 2 'D'),tempo,imagens)
        | ((bot mapa 2 tempo) == Just 'L') = ((move mapa 2 'L'),tempo,imagens)
        | ((bot mapa 2 tempo) == Just 'R') = ((move mapa 2 'R'),tempo,imagens)
        | ((bot mapa 2 tempo) == Just 'B') = ((move mapa 2 'B'),tempo,imagens)
        | otherwise = (mapa,tempo,imagens)

reageBot3 :: Estado -> Estado
reageBot3 (mapa,tempo,imagens)
        | (jogador '3' mapa == "jogador invalido") = (mapa,tempo,imagens)
        | ((bot mapa 3 tempo) == Just 'U') = ((move mapa 3 'U'),tempo,imagens)
        | ((bot mapa 3 tempo) == Just 'D') = ((move mapa 3 'D'),tempo,imagens)
        | ((bot mapa 3 tempo) == Just 'L') = ((move mapa 3 'L'),tempo,imagens)
        | ((bot mapa 3 tempo) == Just 'R') = ((move mapa 3 'R'),tempo,imagens)
        | ((bot mapa 3 tempo) == Just 'B') = ((move mapa 3 'B'),tempo,imagens)
        | otherwise = (mapa,tempo,imagens)

bot :: [String] -> Int -> Int -> Maybe Char
bot mapa player ticks 
                    | (modorisco (grelha mapa) (bombacol mapa) (jogador (intToDigit player) (jogadores mapa))) = procurarposseg (grelha mapa) (jogador (intToDigit player) (jogadores mapa)) (bombacol mapa)
                    | (modoseguro (grelha mapa) (bombacol mapa) (jogador (intToDigit player) (jogadores mapa))) && ( (plantarbomba (tupler (jogador (intToDigit player) (jogadores mapa))) (1 + (length (filter (=='+') (jogador (intToDigit player) mapa)))) mapa) == True ) = Just 'B'
                    | (modoseguro (grelha mapa) (bombacol mapa) (jogador (intToDigit player) (jogadores mapa))) && ( (plantarbomba (tupler (jogador (intToDigit player) (jogadores mapa))) (1 + (length (filter (=='+') (jogador (intToDigit player) mapa)))) mapa) == False ) = moverjogador (grelha mapa) (tupler (jogador (intToDigit player) (jogadores mapa))) (posipumaisprox (tupler (jogador (intToDigit player) (jogadores mapa))) (tuplesdados ((bombas mapa) ++ (flames mapa))) ( ((metdim (head mapa)),(metdim (head mapa))),1000 )) (bombacol mapa) 
                    | otherwise = Nothing



-- Funçao que verifica se plantar uma bomba é util
plantarbomba :: Coordenada -> Raio -> Mapa -> Bool
plantarbomba (coljogador,linjogador) raio mapa = if 
                ((verutil (coljogador,linjogador) raio mapa)==True) && (
                 ( (versegpo (grelha mapa) (bombacol mapa) (coljogador-1,linjogador)) && ( (versegpo (grelha mapa) (bombacol mapa) (coljogador-2,linjogador)) || (versegpo (grelha mapa) (bombacol mapa) (coljogador-1,linjogador+1)) || (versegpo (grelha mapa) (bombacol mapa) (coljogador-1,linjogador-1)) ) ) ||
                  ( (versegpo (grelha mapa) (bombacol mapa) (coljogador+1,linjogador)) && ( (versegpo (grelha mapa) (bombacol mapa) (coljogador+2,linjogador)) || (versegpo (grelha mapa) (bombacol mapa) (coljogador+1,linjogador+1)) || (versegpo (grelha mapa) (bombacol mapa) (coljogador+1,linjogador-1)) ) ) ||
                   ( (versegpo (grelha mapa) (bombacol mapa) (coljogador,linjogador+1)) && ( (versegpo (grelha mapa) (bombacol mapa) (coljogador,linjogador+2)) || (versegpo (grelha mapa) (bombacol mapa) (coljogador-1,linjogador+1)) || (versegpo (grelha mapa) (bombacol mapa) (coljogador-1,linjogador-1)) ) ) ||
                    ( (versegpo (grelha mapa) (bombacol mapa) (coljogador,linjogador-1)) && ( (versegpo (grelha mapa) (bombacol mapa) (coljogador,linjogador-2)) || (versegpo (grelha mapa) (bombacol mapa) (coljogador-1,linjogador-1)) || (versegpo (grelha mapa) (bombacol mapa) (coljogador+1,linjogador-1)) ) )
                     ) then True else False

-- | Função que verifica se colocar uma bomba pode eventualmente destruir algum tijolo
verutil :: Coordenada -> Raio -> Mapa -> Bool
verutil pjogador raio mapa = verefeito mapa pjogador raio

-- | Função que verifica se a explosão duma bomba causa algum efeito
verefeito :: Grelha -> Coordenada -> Raio -> Bool
verefeito mapa (c,l) r = if ( ((verfeitoaux mapa (c,l) r 1 'D')==True) || ((verfeitoaux mapa (c,l) r 1 'U')==True) || ((verfeitoaux mapa (c,l) r 1 'R')==True) || ((verfeitoaux mapa (c,l) r 1 'L')==True) ) then True else False

-- | Função que verifica se a explosão duma bomba causa algum efeito numa determinada direcção(auxiliar de verefeito)
verfeitoaux :: Grelha -> Coordenada -> Raio -> Int -> Direccao -> Bool
verfeitoaux grelha (c,l) r i 'D'
                            | (i==r) = if (verposgrelha grelha (c,l) (0,-i) == Just '?') then True else False
                            | (i<r) && (verposgrelha grelha (c,l) (0,-i) == Just '?') = True
                            | (i<r) && (verposgrelha grelha (c,l) (0,-i) == Just '#') = False
                            | (i<r) && (verposgrelha grelha (c,l) (0,-i) == Just ' ') = verfeitoaux grelha (c,l) r (i+1) 'D'
verfeitoaux grelha (c,l) r i 'U'
                            | (i==r) = if (verposgrelha grelha (c,l) (0,i) == Just '?') then True else False
                            | (i<r) && (verposgrelha grelha (c,l) (0,i) == Just '?') = True
                            | (i<r) && (verposgrelha grelha (c,l) (0,i) == Just '#') = False
                            | (i<r) && (verposgrelha grelha (c,l) (0,i) == Just ' ') = verfeitoaux grelha (c,l) r (i+1) 'U'
verfeitoaux grelha (c,l) r i 'R'
                            | (i==r) = if (verposgrelha grelha (c,l) (i,0) == Just '?') then True else False
                            | (i<r) && (verposgrelha grelha (c,l) (i,0) == Just '?') = True
                            | (i<r) && (verposgrelha grelha (c,l) (i,0) == Just '#') = False
                            | (i<r) && (verposgrelha grelha (c,l) (i,0) == Just ' ') = verfeitoaux grelha (c,l) r (i+1) 'R'
verfeitoaux grelha (c,l) r i 'L'
                            | (i==r) = if (verposgrelha grelha (c,l) (-i,0) == Just '?') then True else False
                            | (i<r) && (verposgrelha grelha (c,l) (-i,0) == Just '?') = True
                            | (i<r) && (verposgrelha grelha (c,l) (-i,0) == Just '#') = False
                            | (i<r) && (verposgrelha grelha (c,l) (-i,0) == Just ' ') = verfeitoaux grelha (c,l) r (i+1) 'L'

-- | Função que verifica se o jogador está livre de explosões na posição actual
modoseguro :: Grelha -> Bombas -> Jogador -> Bool
modoseguro grelha bombas player = if (modorisco grelha bombas player == False) then True else False

-- modorisco (grelha mapa) (bombacol mapa) (jogador (intToDigit player) (jogadores mapa))
modorisco :: Grelha -> Bombas -> Jogador -> Bool
modorisco grelha bombas player = if ((versegpo grelha bombas (tupler player))==False) then True else False

-- | Função que verifica se uma posição esta segura
versegpo :: Grelha -> Bombas -> Coordenada -> Bool
versegpo _ [] _ = True
versegpo grelha (b:bs) jogador = if ((jogador `elem` (tuplesdados (b:bs)))==False) && (verposgrelha grelha jogador (0,0) == Just ' ') && ((bombaslincol (tuplesdados (b:bs)) (jogador) (raiobombas (b:bs)))==False) then True else False

-- | Função que calcula se uma determinada posição está em risco de ocorrer uma explosão
bombaslincol :: [Coordenada] -> Coordenada -> [Raio] -> Bool
bombaslincol [] _ _ = False
bombaslincol ((a,b):xs) (x,y) (r:rs) = if ((a==x) || (b==y)) && ( (distancia (a,b) (x,y)) < (r+1)) then True else bombaslincol xs (x,y) rs

-- | Funçao que calcula a distancia entre 2 posições
distancia :: Coordenada -> Coordenada -> Int
distancia (x,y) (a,b) = (if (x>a) then x-a else a-x) + (if (y>b) then y-b else b-y)

-- | Função que recebe a lista de bombas do estado actual e devolve a lista dos seus raios
raiobombas :: Bombas -> [Raio]
raiobombas [] = []
raiobombas (b:bs) = [1 + (read (takeWhile (/=' ') (drop 1 (dropWhile (/=' ') (reverse b)))) :: Int)] ++ raiobombas bs


-- | Função que decida para onde se deve mover o jogador(ou não efectuar qualquer movimento)
moverjogador :: Grelha -> Coordenada -> Coordenada -> Bombas -> Maybe Char
moverjogador grelha pjogador destino bombas
                    | ( ((verposgrelha grelha pjogador (1,0) ) == Just ' ') && ((versegpo grelha bombas (somapos pjogador (1,0)))==True) && ((distancia pjogador destino) < (distancia (somapos (1,0) pjogador) destino)) ) = Just 'R'
                    | ( ((verposgrelha grelha pjogador (-1,0) ) == Just ' ') && ((versegpo grelha bombas (somapos pjogador (-1,0)))==True) && ((distancia pjogador destino) < (distancia (somapos (-1,0) pjogador) destino)) ) = Just 'L'
                    | ( ((verposgrelha grelha pjogador (0,-1) ) == Just ' ') && ((versegpo grelha bombas (somapos pjogador (0,-1)))==True) && ((distancia pjogador destino) < (distancia (somapos (0,-1) pjogador) destino)) ) = Just 'U'
                    | ( ((verposgrelha grelha pjogador (0,1) ) == Just ' ') && ((versegpo grelha bombas (somapos pjogador (0,1)))==True) && ((distancia pjogador destino) < (distancia (somapos (0,1) pjogador) destino)) ) = Just 'D'
                    | otherwise = Nothing


-- | Função que descobre qual a posição do PowerUp visível mais proximo do jogador, caso exista a menos de 10 movimentos, ou o centro do mapa
posipumaisprox :: Coordenada -> [Coordenada] -> (Coordenada,Int) -> Coordenada
posipumaisprox _ [] (c,_) = c
posipumaisprox pjogador (p:ps) (x,d) = if ((distancia (pjogador) p)<d) then posipumaisprox pjogador ps (p,(distancia (pjogador) p)) else posipumaisprox pjogador ps (x,d)


-- | Função que procura uma posição segura com apenas 3 comandos
procurarposseg :: Grelha -> Jogador -> Bombas -> Maybe Char
procurarposseg grelha jogador bombas 
                    | (((verposgrelha grelha (tupler jogador) (1,0) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (1,0))) ) = Just 'R'
                    | (((verposgrelha grelha (tupler jogador) (-1,0) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (-1,0))) ) = Just 'L'
                    | (((verposgrelha grelha (tupler jogador) (0,-1) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (0,-1))) ) = Just 'U'
                    | (((verposgrelha grelha (tupler jogador) (0,1) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (0,1))) ) = Just 'D'
                    | (((verposgrelha grelha (tupler jogador) (1,-1) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (1,1))) ) = Just 'R'
                    | (((verposgrelha grelha (tupler jogador) (1,1) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (1,-1))) ) = Just 'R'
                    | (((verposgrelha grelha (tupler jogador) (2,0) ) ==  Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (2,0))) ) = Just 'R'
                    | (((verposgrelha grelha (tupler jogador) (-2,0) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (-2,0))) ) = Just 'L'
                    | (((verposgrelha grelha (tupler jogador) (-1,1) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (-1,1))) ) = Just 'L'
                    | (((verposgrelha grelha (tupler jogador) (-1,-1) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (-1,-1))) ) = Just 'L'
                    | (((verposgrelha grelha (tupler jogador) (0,-2) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (0,-2))) ) = Just 'U'
                    | (((verposgrelha grelha (tupler jogador) (1,-1) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (1,-1))) ) = Just 'U'
                    | (((verposgrelha grelha (tupler jogador) (-1,-1) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (-1,-1))) ) = Just 'U'
                    | (((verposgrelha grelha (tupler jogador) (0,2) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (0,2))) ) = Just 'D'
                    | (((verposgrelha grelha (tupler jogador) (1,1) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (1,1))) ) = Just 'D'
                    | (((verposgrelha grelha (tupler jogador) (-1,1) ) == Just ' ') && (versegpo grelha bombas (somapos (tupler jogador) (-1,1))) ) = Just 'D'
                    | otherwise = Nothing

-- | Função que dadas 2 coordenadas devolve a coordenada resultante da soma das duas
somapos :: Coordenada -> Coordenada -> Coordenada
somapos (a,b) (x,y) = (a+x,b+y)

-- | Função que dada a 1ª linha da grelha devolve um inteiro correspondente a metade da dimensão(utíl para descobrir a posição central do mapa)
metdim :: String -> Int
metdim "#" = 0
metdim dim = 1 + metdim (drop 2 dim)


{- | Função que devolve a ocupação duma determinada posição da grelha -}
verposgrelha :: Grelha -> Coordenada -> Coordenada -> Maybe Char
verposgrelha (x:xs) (c,l) (a,b) = if ( (c+a>0) && (l+b>0) && (c+a<(length x)) && (l+b<(length x)) ) then verposgrelhaCol (x:xs) ((c+a),(l+b)) else Nothing where
                    verposgrelhaCol list (c,l) = if (l>length list) then Nothing else verposgrelhaLin (list !! l) c where
                        verposgrelhaLin string c = if (c>length string) then Nothing else Just (string !! c)

-- | Frame rate
fr :: Int
fr = 1

-- | Display mode
dm :: Display
dm = InWindow "Novo Jogo" (900, 900) (0, 0)
