-- GADTs demo


{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE StandaloneDeriving #-}

-- data Option a = Some a
--               | SomeInt Int
--               | None
--   deriving Show

-- addOptions :: Option a -> Option a -> Option a
-- addOptions (Some a1) (Some a2) = error "cant add non ints"
-- -- addOptions (SomeInt i1) (SomeInt i2) = Some $ i1 + i2   -- doesnt compile
-- addOptions None _ = None
-- addOptions _ None = None

data GADTOption a where
  Some :: a -> GADTOption a
  SomeInt :: Int -> GADTOption Int
  None :: GADTOption a

deriving instance Eq a => Eq (GADTOption a)
deriving instance Show a => Show (GADTOption a)

addGADTOptions :: GADTOption a -> GADTOption a -> GADTOption a
addGADTOptions None _ = None
addGADTOptions _ None = None
addGADTOptions (SomeInt i1) (SomeInt i2) = None
addGADTOptions (Some _) (Some _) = error "cant add non ints"
