import Data.Monoid

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a `mappend` tag b) a b


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single m a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i jl@(Append s left right)
  | i < 0 = Nothing
  | i > thisSize = Nothing
  | i < leftSize = indexJ i left
  | otherwise = indexJ (i - leftSize) right
  where
    thisSize = getSize $ size $ s
    leftSize = getSize $ size $ tag left
