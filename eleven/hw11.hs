import Control.Applicative hiding ((*>), mapA, sequenceA, replicateA)
import AParser

(*>) :: Applicative f => f a -> f b -> f b
(*>) = \_ fb -> fb

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = sequenceA . map f

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p


zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []
