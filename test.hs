{-# LANGUAGE RankNTypes #-}

module Main where

data Parser a = Parser
  { p ::
      Char -> -- Input character
      (Consume, Maybe a, Maybe (Parser a))
  }

data Consume = Consumed | Skipped

data PRes a = Finished a | Running
  deriving Show

data Automata = AInt Int | AString String
  deriving Show

instance Functor Parser where
  fmap f (Parser parser) = Parser $ \c ->
    let (cs, re, _) = parser c
    in (cs, f <$> re, Nothing)

instance Applicative Parser where
  pure f = Parser $ \_ -> (Skipped, Just f, Nothing)
  (Parser a) <*> (Parser b) = Parser $ \c ->
    let (as, ar, _) = a c
        (bs, br, _) = b c
        next = Parser $ \c' -> let (bs', br', _) = b c' in (bs', ar <*> br', Nothing)
    in case as of
        Skipped -> (bs, ar <*> br, Nothing)
        Consumed -> (as, undefined, Just next)

char :: Char -> Parser Char
char c = Parser func
  where
    func c'
      | c == c' = (Consumed, Just c, Nothing)
      | otherwise = (Skipped, Nothing, Nothing)

initP :: Parser Automata
initP = pure one <*> char 'x'
  where
    one x = AString [x]


main :: IO ()
main = do
    loop initP
  where
    loop parser = do
      x <- getChar
      let (_, res, nextP) = (p parser) x
      print res
      loop $ case nextP of
          Just next -> next
          Nothing -> initP
