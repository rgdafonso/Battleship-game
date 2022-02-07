module Main where


import Data.Char
import System.IO
import System.Random

-- Tabuleiro do jogo:
type GBoard = [[Char]]
-- Tabuleiro que contem a posicao das minas (Mapa de Minas). True = mina, False = sem mina:
type SBoard = [[Bool]]


gBoard1 :: GBoard
gBoard1 = [['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-']]
gBoard2 :: GBoard
gBoard2 = [['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-']]


sBoard1 :: SBoard
sBoard1 = [[False, False, False, False, False, False, False, False, False],
          [False, False, True, True, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, True, True , True, True, False, False],
          [False, False, False, False, False, True, True, True, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, True, False, False, False, False, False, False]]

sBoard2 :: SBoard
sBoard2 = [[False, False, False, True, True, True, True, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [True, True, True, False, False, False, False, False, False],
          [False, False, False, False, True , True, False, False, False],
          [False, False, False, False, False, True, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False]]


gArr :: Int -> [t] -> t
gArr a b = b!!a


uArr :: Int -> a -> [a] -> [a]
uArr _ _ [] = []
uArr p v (x:xs)
   | p == 0 = v:xs
   | otherwise = x:uArr (p-1) v xs


gPos :: Int -> Int -> [[a]] -> a
gPos l c v = gArr c (v!!l)



uPos :: Int -> Int ->  a -> [[a]] -> [[a]]
uPos l c v t = uArr l (uArr c v (gArr l t)) t



printBoard :: GBoard -> GBoard -> String
printBoard [] [] = ""
printBoard (x:xs) (y:ys) = x ++ "  |  " ++ y ++ "\n" ++ printBoard xs ys

isShip :: Int -> Int -> SBoard -> Bool
isShip l c mb = gPos l c mb

hitBoard :: Int -> Int -> SBoard -> GBoard -> GBoard
hitBoard l c sb gb   
      | isShip l c sb = uPos l c 'X' gb
      | otherwise = uPos l c 'M' gb

countShips :: GBoard -> Int
countShips mb = countHits (concat mb)

countHits :: [Char] -> Int
countHits [] = 0
countHits (x:xs)
      | x == 'X' = 1+ countHits xs
      | otherwise = countHits xs


main :: IO ()
main = do
    putStr "Bem vindo ao jogo Batalha Naval/n Cada jogador tem 4 navios de tamanho 4, 3, 2 e 1\n"
    putStr "Caso navio seja atingido será marcado no tabuleiro com um X caso contratrio M para representar tiro ao mar\n"
    putStr $ printBoard gBoard1 gBoard2
    gameLoop sBoard1 sBoard2 gBoard1 gBoard2
                        


   
gameLoop :: SBoard -> SBoard -> GBoard -> GBoard -> IO ()
gameLoop sb1 sb2 gb1 gb2 = do
   putStr "Jogador 1\n"
   putStr "Digite uma linha: \n"
   linha <- getLine
   putStr "Digite uma coluna: \n"
   coluna <- getLine
   let newGB = hitBoard (read linha) (read coluna) sb2 gb2
   if(countShips(newGB)<10)
      then do
          putStr $ printBoard gb1 newGB
          putStr "Jogador 2\n"
          putStr "Digite uma linha: \n"
          linha2 <- getLine
          putStr "Digite uma coluna: \n"
          coluna2 <- getLine
          let newGB2 = hitBoard (read linha2) (read coluna2) sb1 gb1
          if(countShips(newGB2)<10) 
             then do
                putStr $ printBoard newGB2 newGB
                gameLoop sb1 sb2 newGB2 newGB
          else do
             putStr $ printBoard newGB2 newGB
             putStr "\nParabéns Jogador 2 Você Venceu!!!\n"
             
   else do
      putStr $ printBoard gb1 newGB
      putStr "\nParabéns Jogador 1 Você Venceu!!!\n"
      
 