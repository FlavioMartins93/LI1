
{- | 
O objectivo desta tarefa é, dada uma descrição do estado do jogo e um comando de um dos
jogadores, determinar o efeito desse comando no estado do jogo. Cada jogador é identificado
por um dígito entre 0 e 3 e os comandos podem ser os caracteres ‘U’ (ir para cima), ‘D’ (ir
para baixo), ‘L’ (ir para a esquerda), ‘R’ (ir para a direita) e ‘B’ (colocar uma bomba).

O estado do jogo é representado usando o formato descrito na tarefa anterior, mais uma linha a
descrever o estado de cada bomba colocada no mapa, e uma linha a descrever o estado de
cada jogador. Uma bomba é identificada pelo caracter ‘*’ sendo listada depois a sua posição,
qual o jogador que a colocou, qual o seu raio de acção e quantos instantes de tempo faltam
para explodir. Um jogador é identificado pelo seu dígito sendo listada depois a sua posição e os
power ups que entretanto acumulou. O jogo suporta um máximo de 4 jogadores. Bombas e
jogadores devem aparecer ordenados por posição e identificador, respectivamente.

Sempre que um comando não possa ser executado o resultado deverá ser o mesmo estado.
Note que na mesma célula podem coexistir mais do que um jogador e um jogador pode
também estar posicionado numa célula onde se encontra uma bomba. No entanto, na mesma
célula não pode estar mais do que uma bomba. 

Para realizar esta tarefa decidimos dividir o input em 5 partes de forma a facilitar o seu processamento,
sendo estas 5 partes as seguintes:

- grelha de jogo
- powerUPs bomba
- powerUPs flames
- bombas colocadas
- jogadores

-}
module Main where

import Data.Char
import Data.List
import System.Environment

{- Função main que transforma o ficheiro de testes no input pretendido para a tarefa -}
main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"

{- | A principal função do nosso mapa, esta função 
para os comandos 'D','U','L','R' verifica se a posição para a qual o jogador se irá mover está livre,
e caso esteja ocupada retorna o input recebido, caso esteja livre move o jogador nessa direcção e
verifica se na posição destino existe algum power up e, caso exista adiciona-o ao jogador,
para o comando 'B' verifica se a posição onde está o jogador não tem nenhuma bomba colocada, 
compara-se o numero de bombas já colocadas pelo jogador com o numero de powerUps bomba que o jogador tem e, 
caso o jogador possa colocar a bomba, coloca a bomba, caso contrário devolve o input recebido.-}
move :: [String] -> Int -> Char -> [String]
move mapa j c
    | ( mapa == [] ) = error "erro no mapa"
    | ( j < 0 ) || ( j > 3 ) = error "numero de jogador invalido"
    | ( c == 'B' ) = if (verposibomba (jogador (intToDigit j) mapa) (bombacol mapa)) && ((verjogcolbom j (bombacol mapa) ) < (1 + (length (filter (=='+') (jogador (intToDigit j) (jogadores mapa))))))
                        then grelha mapa ++ bombas mapa ++ flames mapa ++ inserirbomba (criarbomba (tupler (jogador (intToDigit j) mapa)) j (1 + (length (filter (=='!') (jogador (intToDigit j) mapa)))) 10) (bombacol mapa) ++ (jogadores mapa)
                        else mapa
    | ( c == 'D' ) = if (verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (0,1))
                        then ( if((verPU (moverJogador (jogador (intToDigit j) mapa) (0,1)) ((bombas mapa) ++ (flames mapa)))==False) 
                                  then grelha mapa ++ bombas mapa ++ flames mapa ++ bombacol mapa ++ insert (moverJogador (jogador (intToDigit j) mapa) (0,1)) (removerjogadormovido j (jogadores mapa))
                                  else grelha mapa ++ remPU (moverJogador (jogador (intToDigit j) mapa) (0,1)) ((bombas mapa) ++ (flames mapa)) ++ bombacol mapa ++ insert (addPU (moverJogador (jogador (intToDigit j) mapa) (0,1)) ((bombas mapa) ++ (flames mapa))) (removerjogadormovido j (jogadores mapa)) )                     
                       else mapa
    | ( c == 'U' ) = if (verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (0,(-1))) 
                        then ( if((verPU (moverJogador (jogador (intToDigit j) mapa) (0,-1)) ((bombas mapa) ++ (flames mapa)))==False) 
                                  then grelha mapa ++ bombas mapa ++ flames mapa ++ bombacol mapa ++ insert (moverJogador (jogador (intToDigit j) mapa) (0,-1)) (removerjogadormovido j (jogadores mapa))
                                  else grelha mapa ++ remPU (moverJogador (jogador (intToDigit j) mapa) (0,-1)) ((bombas mapa) ++ (flames mapa)) ++ bombacol mapa ++ insert (addPU (moverJogador (jogador (intToDigit j) mapa) (0,-1)) ((bombas mapa) ++ (flames mapa))) (removerjogadormovido j (jogadores mapa)) )
                        else mapa
    | ( c == 'L' ) = if (verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) ((-1),0)) 
                        then( if((verPU (moverJogador (jogador (intToDigit j) mapa) ((-1),0)) ((bombas mapa) ++ (flames mapa)))==False) 
                                  then grelha mapa ++ bombas mapa ++ flames mapa ++ bombacol mapa ++ insert (moverJogador (jogador (intToDigit j) mapa) ((-1),0)) (removerjogadormovido j (jogadores mapa))
                                  else grelha mapa ++ remPU (moverJogador (jogador (intToDigit j) mapa) ((-1),0)) ((bombas mapa) ++ (flames mapa)) ++ bombacol mapa ++ insert (addPU (moverJogador (jogador (intToDigit j) mapa) ((-1),0)) ((bombas mapa) ++ (flames mapa))) (removerjogadormovido j (jogadores mapa)) )
                        else mapa
    | ( c == 'R' ) = if (verposigrelha (grelha mapa) (tupler (jogador (intToDigit j) mapa)) (1,0)) 
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
jogador _ [] = error "jogador invalido"
jogador x (y:ys) = if ( x == head y ) then y else jogador x ys

{- | Função para adquirir a posicao no mapa de um PowerUP, jogador ou um bomba colocada -}
tupler :: String -> (Int,Int)
tupler [] = error "erro a criar tuple"
tupler list = (read (takeWhile (/=' ') (drop 1 (dropWhile (/=' ') list)) ) :: Int, read ( takeWhile (/=' ') (drop 1 ( dropWhile (/=' ') (drop 1 (dropWhile (/=' ') list)) ))) :: Int )

{- | Função que utilizamos para transformar a lista de bombas ou flames na lista das suas coordenadas -}
tuplesdados :: [String] -> [(Int,Int)]
tuplesdados [] = []
tuplesdados (x:xs) = [tupler x] ++ tuplesdados xs

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

{- | Função que verifica se uma determinada posicao da grelha esta ocupada por alguma parede ou tijolo -}
verposigrelha :: [String] -> (Int,Int) -> (Int,Int) -> Bool
verposigrelha (x:xs) (c,l) (a,b) = verposigrelhaCol (x:xs) ((c+a),(l+b)) where
                    verposigrelhaCol list (c,l) = verposigrelhaLin (list !! l) c where
                        verposigrelhaLin string c = if ((string !! c) /= ' ') then False else True

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
