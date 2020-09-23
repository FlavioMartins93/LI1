{- | 
O objectivo desta tarefa é implementar um mecanismo de geração de mapas.
O input será a dimensão do mapa (número ímpar maior ou igual a 5) e um número inteiro positivo para usar como semente num gerador pseudo-aleatório. 
O output deverá ser um mapa impresso no formato:


Primeiro a grelha é descrita usando o caracter ‘#’ para representar pedra, ‘?’ para representar tijolo, e ‘ ‘ para
representar uma célula vazia. Depois deverão ser listados os power ups escondidos, um por
linha. Para cada power up deve ser indicado o seu tipo (o caracter ‘+’ representa Bombs e o
caracter ‘!’ representa Flames) e as coordenadas onde se encontra (primeiro horizontal e
depois vertical). Os power ups Bombs devem ser listados antes dos Flames, e devem ser
ordenados aparecendo primeiro os que aparecem mais acima e mais à esquerda. 

Para realizar esta tarefa decidimos criar o mapa em 3 fases

 - Inicialmente apenas com as suas paredes e o caracter 'v' nos locais onde iria ser preenchido com espaços livres (' ') ou tijolos('?')

 - A seguir substituindo os 'v's pelos espaços, tijolos, bombas('+') ou flames('!') (a ideia de colocar inicialmente as bombas e flames no mapa era para facilitar a encontrar a localização das mesmas)

 - Por fim a grelha final substituindo as bombas('+') e flames(!) por tijolos

-}
module Main where

import System.Random
import System.Environment
import Text.Read
import Data.Maybe

{- Função main que transforma o ficheiro de testes no input pretendido para a tarefa -}
main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"


{- | A principal função do nosso mapa, que é a junção de duas funções criadas, uma que gera a grelha e outra que gera a localição dos powerUPs -}
mapa :: Int -> Int -> [String]
mapa d s 
        | even d || d < 5 = error "dimensao invalida"
        | otherwise = subPUmapa ( subsVmapa (criarMapaV d) (take (contV (criarMapaV d)) $ randomRs (0,99) (mkStdGen s)) )  ++ mapPU (subsVmapa (criarMapaV d) (take (contV (criarMapaV d)) $ randomRs (0,99) (mkStdGen s)))

{- | Função que cria a nossa grelha com 'v's nos quadrados a substituir por valores da lista -}
criarMapaV :: Int -> [String]
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
subsVmapa :: [String] -> [Int] -> [String]
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
