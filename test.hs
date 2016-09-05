{-# LANGUAGE RankNTypes #-}

module Main where

import Debug.Trace (trace)

data Parser a = Parser
  { p :: forall r.
      Char -> -- Input character
      (Consume, Maybe a, Maybe (Parser a))
  }

data Consume = Consumed | Skipped | Errored
  deriving Show

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
    let (as, ar, an) = a c
        (bs, br, bn) = b c
        jr = case ar of
          Just x -> x
          Nothing -> error "no"
        next = Parser $ \c' ->
          let (bs', br', bi') = b c'
          in case bi' of
            Just jbi' -> (bs', ar <*> br', Just (jr <$> jbi'))
            Nothing -> (bs', ar <*> br', Nothing)
    in case as of
        Skipped -> case bs of
          Errored -> (Errored, Nothing, Nothing)
          _ -> case bn of
            Just jbn -> trace " - With Next" (bs, ar <*> br, Just (jr <$> jbn))
            Nothing ->  trace " - Skipped" (bs, ar <*> br, Nothing)
        Consumed -> case an of
          Just an' -> trace (" - Consumed: " ++ [c]) (as, Nothing, Just (an' <*> (Parser b)))
          Nothing ->  trace (" - Consumed: " ++ [c]) (as, Nothing, Just next)
        Errored -> (Errored, Nothing, Nothing)

char :: Char -> Parser Char
char c = Parser func
  where
    func c'
      | c == c' = trace (" - char: " ++ [c]) (Consumed, Just c, Nothing)
      | otherwise = trace (" - char: need '" ++ [c] ++ "' but '" ++ [c'] ++ "'")  (Errored, Nothing, Nothing)

initP :: Parser Automata
initP = pure five <*> char 'a' <*> char 'b' <*> char 'c' <*> char 'd' <*> char 'e'
  where
    five a b c d e = AString [a, b, c, d, e]

main :: IO ()
main = do
    loop initP
  where
    loop parser = do
      x <- getChar
      putStrLn "-----------"
      putStrLn $ "Input: " ++ [x]
      let (a, res, nextP) = (p parser) x
      print res
      print (a, res)
      case nextP of
          Just next -> loop next
          Nothing -> error "FInished"
