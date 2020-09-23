{- | 
O objectivo desta tarefa é, dada uma descrição do estado do jogo (idêntica à utilizada na tarefa2)
 implementar um mecanismo de compressão / descompressão que permita poupar
caracteres e, desta forma, poupar espaço em disco quando o estado do jogo for gravado
(permitindo, por exemplo, fazer pausa durante o jogo com o objectivo de o retomar mais tarde).

Para comprimir o estado actual do jogo decidimos:

- Guardar a dimensao do mapa

- Remover todas as paredes da nossa grelha

- Manter as powerUps, bombas colocadas e os jogadores(sem os 2 primeiros caracteres de cada elemento e separados por vírgulas os elementos do mesmo tipo)

Para separar a dimensão da grelha sem paredes usamos o caracter 'd'

Para separar a grelha sem paredes dos PU bomba usamos o caracter '+'

Para separar os powerUP bomba dos powerUP flames usamos o caracter '!'

Para separar os powerUP flames das bombas colocadas o caracter '*'

Para separar as bombascolocadas dos jogadores o caracter 'j'
-}
module Main where

import Data.Char
import Data.List
import System.Environment

{- Função main que transforma o ficheiro de testes no input pretendido para a tarefa -}
main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"

{- | Função que transforma o estado actual do tipo [String] numa String em que se guarda a dimensão, a grelha sem paredes e a lista de powerUps, bombas colocadas e jogadores -}
encode :: [String] -> String
encode l = show (guardardimensao l) ++ "d" ++
                 removerParedes (grelha l) ++
                       "+" ++ separar (bombas l) ++ "!" ++ separar (flames l) ++ "*" ++ separar (bombacol l) ++ "j" ++ separar (jogadores l)

{- | Função que devolve a String guardada ao seu estado inicial -}
decode :: String -> [String]
decode l = recolocarParedes (read (takeWhile (/='d') l) :: Int) 0 ( drop 1 (dropWhile (/='d') l) ) ++
                    juntar "+ " (takeWhile (/='!') (drop 1 (dropWhile (/='+') l))) ++
                          juntar "! " (takeWhile (/='*') (drop 1 (dropWhile (/='!') l))) ++
                                juntar "* " (takeWhile (/='j') (drop 1 (dropWhile (/='*') l))) ++
                                      juntarjogadores "0 " (drop 1 (dropWhile (/='j') l))

{- | Função que calcula a dimensão da grelha de jogo-}
guardardimensao :: [String] -> Int
guardardimensao (x:xs) = length x

{- | Função que remove as paredes da grelha de jogo-}
removerParedes :: [String] -> String
removerParedes [] = []
removerParedes ([]:xs) = removerParedes xs
removerParedes (x:xs) = if ((head x)=='#') then removerParedes ((drop 1 x):xs) else (head x) : removerParedes ((drop 1 x):xs)

{- | Função que recoloca as paredes na grelha -}
recolocarParedes :: Int -> Int -> String -> [String]
recolocarParedes d l list 
        | (l == (d-1)) = [take d (repeat '#')] 
        | (l == 0) = [take d (repeat '#')] ++ recolocarParedes d (l+1) list
        | (l > 0) && (l<(d-1)) && (odd l) = ["#" ++ take (d-2) list ++ "#"] ++ recolocarParedes d (l+1) (drop (d-2) list)
        | (l > 0) && (l<(d-1)) && (even l) = [recolocarParedesLinPar d 0 list] ++ recolocarParedes d (l+1) (drop ((d-2) -  (div (d-2) 2)) list) where 
                 recolocarParedesLinPar d c list
                            | (c == (d-1)) = "#"
                            | (c == 0) = "#" ++ recolocarParedesLinPar d (c+1) list
                            | (c>0) && (c<(d-1)) && (odd c) = [head list] ++ recolocarParedesLinPar d (c+1) (drop 1 list)
                            | (c>0) && (c<(d-1)) && (even c) = "#" ++ recolocarParedesLinPar d (c+1) list


{- | Função que transforma as bombas, flames, bombas colocadas ou os jogadores numa String com ',' a separar cada elemento -}
separar :: [String] -> String
separar [] = ""
separar [x] = (drop 2 x)
separar (x:xs) = (drop 2 x) ++ "," ++ (separar xs) 

{- | Função que transforma as powerUps e bombas colocadas numa lista do tipo [String] -}
juntar :: String -> String -> [String]
juntar _ [] = []
juntar a list = (a ++ (takeWhile (/=',') list)) : (juntar a (drop 1 (dropWhile (/=',') list)))

{- | Função que transforma os jogadores numa lista do tipo [String] -}
juntarjogadores :: String -> String -> [String]
juntarjogadores _ [] = []
juntarjogadores a list
                  | (a=="0 ") = (a ++ (takeWhile (/=',') list)) : (juntarjogadores "1 " (drop 1 (dropWhile (/=',') list)))
                  | (a=="1 ") = (a ++ (takeWhile (/=',') list)) : (juntarjogadores "2 " (drop 1 (dropWhile (/=',') list)))
                  | (a=="2 ") = (a ++ (takeWhile (/=',') list)) : (juntarjogadores "3 " (drop 1 (dropWhile (/=',') list)))
                  | (a=="3 ") = [(a ++ (list))]


{- | Função que recebe o input e devolve apenas a grelha de jogo -}
grelha :: [String] -> [String]
grelha [] = []
grelha (x:xs) = if ( head x == '#' ) then [x] ++ grelha xs else grelha xs

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