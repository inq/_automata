module Main where

import Data.Char (digitToInt)

data Parser a = Parser { p :: Char -> Parser a -> (Maybe a, Parser a) }

data Automata = AInt Int
  deriving Show

csi :: Parser Automata
csi = Parser func
  where
    func 'e' _ = (Nothing, csi')
    func _ i = (Nothing, i)

csi' :: Parser Automata
csi' = Parser func
  where
    func '[' i = (Nothing, num 0)
    func _ i = (Nothing, i)

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
