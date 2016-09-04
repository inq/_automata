module Main where

import Data.Char (digitToInt)

data Parser a = Parser
  { p ::
      Char -> -- Input character
      Parser a -> -- Initial Parser
      (Maybe a, Parser a)
  }

data Automata = AInt Int
  deriving Show

char :: Char -> Parser Automata -> Parser Automata
char c next = Parser func
  where
    func c' i
      | c == c' = (Nothing, next)
      | otherwise = (Nothing, i)

string :: String -> Parser Automata -> Parser Automata
string (c:cs) next = Parser func
  where
    func c' i
      | c == c' = (Nothing, string cs next)
      | otherwise = (Nothing, i)
string [] next = next

csi :: Parser Automata
csi = string "e[" (num 0)

num :: Int -> Parser Automata
num acc = Parser func
  where
    func c i
      | c >= '0' && c <= '9' = (Nothing, num (acc * 10 + digitToInt c))
      | otherwise = p (finalize acc) c i

finalize :: Int -> Parser Automata
finalize acc = Parser func
  where
    func 'R' i = (Just $ AInt acc, i)
    func _ i = (Nothing, i)

main :: IO ()
main = do
    loop initParser
  where
    initParser = csi
    loop parser = do
      x <- getChar
      let (res, next) = p parser x initParser
      print res
      loop next
