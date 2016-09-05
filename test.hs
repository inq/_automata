{-# LANGUAGE RankNTypes #-}

module Main where

data Parser a = Parser
  { p ::
      Char -> -- Input character
      (Maybe a, Parser a) -- Result, Next
  } | Nope | Func a

data PRes a = Finished a | Running
  deriving Show

data Automata = AInt Int | AString String
  deriving Show

instance Functor Parser where
  fmap f (Parser parser) = Parser $ \c ->
    let (re, ne) = parser c
    in (f <$> re, f <$> ne)
  fmap _ Nope = Nope
  fmap f (Func a) = Func $ f a

(|||) :: Parser a -> Maybe a -> Parser a
Nope ||| Just x = pure x
a ||| _ = a

instance Applicative Parser where
  pure f = Func f
  Func a <*> Func b = Func $ a b
  Func a <*> Parser b = Parser $ \c ->
    let (br, bn) = b c in (a <$> br, a <$> bn)
  Parser a <*> Func b = Parser $ \c ->
    let (ar, an) = a c in (ar <*> pure b, an <*> pure b)
  (Parser a) <*> (Parser b) = Parser $ \c ->
    let (ar, an) = a c in (Nothing, (an ||| ar) <*> Parser b)
  _ <*> _ = Nope

char :: Char -> Parser Char
char c = Parser func
  where
    func c'
      | c == c' = (Just c, Nope)
      | otherwise = (Nothing, Nope)

string :: String -> Parser String
string (c:cs) = pure (:) <*> char c <*> string cs
string [] = pure []

initP :: Parser Automata
initP = pure AString <*> (pure three <*> char 'x' <*> char 'y' <*> char 'z')
  where
    three x y z = [x, y, z]

initQ :: Parser Automata
initQ = pure AString <*> string "abc"

main :: IO ()
main = do
    loop initQ
  where
    loop parser = do
      x <- getChar
      putStrLn "-----------"
      putStrLn $ "Input: " ++ [x]
      let (res, nextP) = (p parser) x
      print res
      case nextP of
          Nope -> error "Finished"
          next -> loop next
