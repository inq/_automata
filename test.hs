{-# LANGUAGE RankNTypes #-}

module Main where

type Success a r = a -> PRes r

data Parser a = Parser
  { p :: forall r.
      Char -> -- Input character
      Success a r ->
      PRes r
  }

data PRes a = Finished a | Running | Failed
  deriving Show

data Automata = AInt Int | AString String
  deriving Show

successK :: Success a a
successK a = Finished a

instance Functor Parser where
  fmap f (Parser parser) = Parser $ \c succ' ->
    parser c (succ' . f)

instance Applicative Parser where
  pure x = Parser $ \_ succ' -> succ' x
  (Parser a) <*> (Parser b) = Parser $ \c succ' ->
    b c (\x -> a '_' (\f -> succ' (f x)))

char :: Char -> Parser Char
char c = Parser func
  where
    func c' succ'
      | c == c' = succ' c
      | otherwise = Failed

initP :: Parser Automata
initP = (two <$> char 'x') <*> char 'y'
  where
    two x y = AString [x, y]


main :: IO ()
main = do
    loop initP
  where
    loop parser = do
      x <- getChar
      let res = (p parser) x successK
      print res
