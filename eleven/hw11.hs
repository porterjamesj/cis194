import Control.Applicative
import Data.Char
import AParser

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)


-- Parsing S Expressions

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> parseList) <* spaces
  where parseAtom = A <$> (parseInt <|> parseIdent)
        parseInt = N <$> posInt
        parseIdent = I <$> ident
        parseList = Comb <$> (char '(' *> oneOrMore parseSExpr <* char ')')
