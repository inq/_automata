module Main where

data Parser a = Parser { p :: Char -> Parser a -> (Maybe a, Parser a) }

data Automata = CSI
  deriving Show

csi :: Parser Automata
csi = Parser func
  where
    func 'e' i = (Nothing, csi')
    func _ i = (Nothing, i)

csi' :: Parser Automata
csi' = Parser func
  where
    func '[' i = (Just CSI, i)
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
