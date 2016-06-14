import Data.Char
import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


first :: (a -> b) -> (a,c) -> (b,c)
first f (one, two) = (f one, two)


instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p


instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))

  (<*>) (Parser f) (Parser a) = Parser $ \s ->
    case f s of
      Just (f', rest) ->
        case a rest of
          Just (a' , rest') -> Just (f' a', rest')
          Nothing -> Nothing
      Nothing -> Nothing

discard :: Parser (a -> ())
discard = pure (\_ -> ())

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = discard <*> abParser

intPair :: Parser [Integer]
intPair = makePair <$> posInt <*> char ' ' <*> posInt
  where makePair = \i _ j -> [i,j]


-- Alternative instance and exercises

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  (<|>) (Parser p1) (Parser p2) = Parser $ \s ->
    case p1 s of
      Just (a, rest) -> Just (a, rest)
      Nothing -> case p2 s of
        Just (a', rest') -> Just (a', rest')
        Nothing -> Nothing


intOrUppercase :: Parser ()
intOrUppercase = (discard <*> posInt) <|> (discard <*> upperCase)
  where upperCase = satisfy (\c -> isUpper c)
