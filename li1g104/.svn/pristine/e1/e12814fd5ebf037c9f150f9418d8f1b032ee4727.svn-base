module Tarefa6_li1g104 where


import System.Random
import System.Environment
import Text.Read
import Data.Maybe
import Data.Char
import Data.List

-- | Função principal do nosso bot, recebe o estado de jogo actual, o nº do jogador e o tempo de jogo e devolve uma jogada, 'B'(plantar bomba) ou mover o jogador('U','D','L','R')
bot :: [String] -> Int -> Int -> Maybe Char
bot mapa player ticks 
                    | (modorisco (grelha mapa) (bombacol mapa) (jogador (intToDigit player) (jogadores mapa))) = procurarposseg (grelha mapa) (jogador (intToDigit player) (jogadores mapa)) (bombacol mapa)
                    | (modoseguro (grelha mapa) (bombacol mapa) (jogador (intToDigit player) (jogadores mapa))) && ( (plantarbomba (tupler (jogador (intToDigit player) (jogadores mapa))) (1 + (length (filter (=='+') (jogador (intToDigit player) mapa)))) mapa) == True ) = Just 'B'
                    | (modoseguro (grelha mapa) (bombacol mapa) (jogador (intToDigit player) (jogadores mapa))) && ( (plantarbomba (tupler (jogador (intToDigit player) (jogadores mapa))) (1 + (length (filter (=='+') (jogador (intToDigit player) mapa)))) mapa) == False ) = moverjogador (grelha mapa) (tupler (jogador (intToDigit player) (jogadores mapa))) (posipumaisprox (tupler (jogador (intToDigit player) (jogadores mapa))) (tuplesdados ((bombas mapa) ++ (flames mapa))) ( ((metdim (head mapa)),(metdim (head mapa))),1000 )) (bombacol mapa) 
                    | otherwise = Nothing
type Direccao = Char
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


-- | Função que conta quantas bombas um jogador já tem colocadas no mapa -}
verjogcolbom :: Int -> [String] -> Int
verjogcolbom j [] = 0
verjogcolbom j (x:xs) = if ( j == (read (takeWhile (/=' ') (drop 1 (dropWhile (/=' ') (drop 1 (dropWhile (/=' ') (drop 2 x)))))) :: Int)) then 1 + (verjogcolbom j xs) else 0 + (verjogcolbom j xs)

{- | Função para adquirir a posicao no mapa de um PowerUP, jogador ou um bomba colocada -}
tupler :: String -> Coordenada
tupler [] = error "erro a criar tuple"
tupler list = (read (takeWhile (/=' ') (drop 1 (dropWhile (/=' ') list)) ) :: Int, read ( takeWhile (/=' ') (drop 1 ( dropWhile (/=' ') (drop 1 (dropWhile (/=' ') list)) ))) :: Int )

{- | Função que utilizamos para transformar a lista de bombas ou flames na lista das suas coordenadas -}
tuplesdados :: [String] -> [Coordenada]
tuplesdados [] = []
tuplesdados (x:xs) = [tupler x] ++ tuplesdados xs

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
jogador _ [] = error "jogador invalido"
jogador x (y:ys) = if ( x == head y ) then y else jogador x ys

{- | Função que devolve a ocupação duma determinada posição da grelha -}
verposigrelha :: Grelha -> Coordenada -> Coordenada -> Char
verposigrelha (x:xs) (c,l) (a,b) = verposigrelhaCol (x:xs) ((c+a),(l+b)) where
                    verposigrelhaCol list (c,l) = verposigrelhaLin (list !! l) c where
                        verposigrelhaLin string c = string !! c
