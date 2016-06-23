import Prelude hiding (sequence,)

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

-- [10,20,30] >>= addOneOrTwo

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
-- sequence (ma:mas) =
--   ma >>= \a ->
--   sequence mas >>= \as ->
--   return (a:as)

sequence (ma:mas) = do
  a <- ma
  as <- sequence mas
  return (a:as)
