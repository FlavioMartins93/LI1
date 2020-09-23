{- | O objectivo desta tarefa é,  dada uma descrição do estado do jogo, determinar o efeito da
passagem de um instante de tempo nesse estado. O estado do jogo será representado no
mesmo formato usado na Tarefa 2 da primeira fase do projecto.
Cada jogo tem uma duração fixa. Para forçar os jogadores a efectuar acções, num mapa de
dimensão n, quando faltarem (n-2)^2 instantes de tempo o mapa começa a fechar-se com
blocos de pedra num efeito de espiral que começa na posição 1 1.

Para realizar esta Tarefa decidimos dividir a tarefa em 2 partes, simulaão da explosão das bombas e similação do efeito espiral.

Para simular o efeito da explosão de bombas criamos uma função que 
simula o efeito da explosão de bombas, que explode nas quatro direcções ('D','U','R','L')
com um dado raio e que irá ser travada por qualquer parede, powerUP
 ou tijolo(removendo o tijolo e revelando, caso exista, o powerUP escondido pelo menos),
além disto, nas posições onde se dá a explosão irá eliminar todos os jogadores que estejam nessa posição e, 
caso exista, irá colocar o tempo para explodir
da bomba dessa possição a 1(de modo a explodir no instante seguinte).
Para simular o efeito espiral de fechar o mapa criamos uma função que, 
recebendo o mapa e o tempo de jogo, coloca paredes de forma espiral
de modo a fechar o mapa e remove tudo que exista na posição que se deve fechar num dado instante de tempo.
-}
module Main where

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe

type Mapa = [String] -- ^ inclui todos os elementos
type Grelha = [String] -- ^ apenas a grelha de jogo (tijolos e paredes) (sem powerups,bombas e jogadores)
type Elementos = [String] -- ^ inclui todo a informaçao menos a grelha
type PUbomba = String
type PUflame = String
type Bomba = String
type Raio = Int        -- ^ Raio de acção duma bomba
type Jogador = String
type Linha = Int
type Coluna = Int
type Coordenada = (Coluna,Linha)
type Tempo = Int
type Dimensao = Int

-- | Função principal desta Tarefa, recebe como input o mapa do jogo e o tempo que falta para acabar o jogo e devolve o mapa com o efeito(caso exista) da passagem do tempo
avanca :: [String] -> Int -> [String]
avanca mapa t = if (t< ((length (head mapa))-2)^2 )
                  then  ( expbombas (bombasaexp (bombacol mapa)) (fecharmapa (((length (head mapa))^2)-t) ((grelha mapa) ++ (bombas mapa) ++ (flames mapa) ++ (decbombas (bombacol mapa)) ++ (jogadores mapa))))
                  else expbombas (bombasaexp (bombacol mapa)) ((grelha mapa) ++ (bombas mapa) ++ (flames mapa) ++ (decbombas (bombacol mapa)) ++ (jogadores mapa))

main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"


{- | Função que recebe o input e devolve apenas a grelha de jogo -}
grelha :: Mapa -> Grelha
grelha [] = []
grelha (x:xs) = if ( head x == '#' ) then [x] ++ grelha xs else []


{- | Função que recebe o input e devolve a lista de bombas -}
bombas :: Mapa -> [PUbomba]
bombas [] = []
bombas (x:xs) = if ( head x == '+' ) then [x] ++ bombas xs else bombas xs


{- | Função que recebe o input e devolve a lista de flames -}
flames :: Mapa -> [PUflame]
flames [] = []
flames (x:xs) = if ( head x == '!' ) then [x] ++ flames xs else flames xs


{- | Função que recebe o input e devolve a lista das bombas colocadas -}
bombacol :: Mapa -> [Bomba]
bombacol [] = []
bombacol (x:xs) = if (head x == '*') then [x] ++ bombacol xs else bombacol xs


{- | Função que recebe o input e devolve a lista jogadores -}
jogadores :: Mapa-> [Jogador]
jogadores [] = []
jogadores (x:xs) = if ( (head x == '0') || (head x == '1') || (head x == '2') || (head x == '3') ) then [x] ++ jogadores xs else jogadores xs

{- | Função que pega na lista de jogadores e devolve apenas um jogador -}
jogador :: Char -> [Jogador] -> Jogador
jogador _ [] = error "jogador invalido"
jogador x (y:ys) = if ( x == head y ) then y else jogador x ys

{- | Função para adquirir a posicao no mapa de um PowerUP, jogador ou um bomba colocada -}
tupler :: String -> Coordenada
tupler [] = error "erro a criar tuple"
tupler list = (read (takeWhile (/=' ') (drop 1 (dropWhile (/=' ') list)) ) :: Int, read ( takeWhile (/=' ') (drop 1 ( dropWhile (/=' ') (drop 1 (dropWhile (/=' ') list)) ))) :: Int )

{- | Função que utilizamos para transformar a lista de bombas ou flames na lista das suas coordenadas -}
tuplesdados :: [String] -> [Coordenada]
tuplesdados [] = []
tuplesdados (x:xs) = [tupler x] ++ tuplesdados xs

{- | Função que verifica se uma determinada posicao da grelha esta ocupada por alguma parede ou tijolo -}
verposigrelha :: Grelha -> Coordenada -> Coordenada -> Char
verposigrelha (x:xs) (c,l) (a,b) = verposigrelhaCol (x:xs) ((c+a),(l+b)) where
                    verposigrelhaCol list (c,l) = verposigrelhaLin (list !! l) c where
                        verposigrelhaLin string c = string !! c


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
                ((grelha mapa) ++ (bombas mapa) ++ (flames mapa) ++ (bombacol mapa) ++ (removerjogpos (tupler x) (jogadores mapa))) )))

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
                                else  explodir (a,b) (c+a,l+b) (ra-1) ((grelha mapa) ++ (bombas mapa) ++ (flames mapa) ++ (tempobombaum (a+c,b+l) (bombacol mapa)) ++ (eliminarjogatingidos (a+c,b+l) (jogadores mapa)))   )))) 
        else mapa

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
