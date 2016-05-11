import Employee
import Data.Monoid
import Data.Tree
import Data.List


glCons :: Employee -> GuestList -> GuestList
glCons employee (GL employees fun) =
  GL (employee : employees) (fun + empFun employee)


instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)


moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 >= f2 = gl1
  | otherwise = gl2


treeFold :: (a -> [b] -> b) -> Tree a -> b -> b
treeFold f (Node label sf) acc = f label (map thisFold sf)
  where thisFold = \tree -> treeFold f tree acc


nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
-- the fst GuestList is optimal if we do invite the boss,
-- the snd GuestList is optimal if we don't
nextLevel boss subtrees = (withBoss, withoutBoss)
  where
    withBoss = glCons boss (mconcat $ map snd subtrees)
    withoutBoss = mconcat $ map fst subtrees


maxFun :: Tree Employee -> GuestList
maxFun empTree =
  uncurry moreFun $ treeFold nextLevel empTree mempty


pretty :: GuestList -> String
pretty (GL emps fun) = intercalate "\n" lines
  where
    header = "Total fun: " ++ show fun
    lines = header : map empName emps


main :: IO ()
main =
  readFile "company.txt" >>=
  putStrLn . pretty . maxFun . read
