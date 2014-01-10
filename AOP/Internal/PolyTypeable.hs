{-# LANGUAGE CPP,
             ScopedTypeVariables,
             FlexibleInstances,
             OverlappingInstances,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleContexts,
             UndecidableInstances,
             TypeFamilies,
             IncoherentInstances
  #-}

-- Our modified version of PolyTypeable. There was lost information when obtaining the 
-- type representation of values constructed using monad transformers.

--
-- This amazing piece of code does that Data.Typeable.typeOf does,
-- but also does it for polymorphic functions.
-- This is Oleg type magic at its best.

module AOP.Internal.PolyTypeable (
       PolyTypeable (..),
       Analyze (..),
       TVar, TCon0, TCon1, TCon2, TCon3, TCon4, TCon5,      

) where
import Data.Typeable
import Data.Int
import Data.Word

----------------------------------
-- Type analysis, classifies each type constructor and type variables.

data TVar a
data TCon0 c
data TCon1 (c :: * -> *) a1
data TCon2 (c :: * -> * -> *) a1 a2
data TCon3 (c :: * -> * -> * -> *) a1 a2 a3
data TCon4 (c :: * -> * -> * -> * -> *) a1 a2 a3 a4
data TCon5 (c :: * -> * -> * -> * -> * -> * ) a1 a2 a3 a4 a5

class Analyze a b | a -> b

#define BASE(t) instance (r ~ TCon0 t)  => Analyze t r
BASE(())
BASE(Bool)
BASE(Char)
BASE(Ordering)
BASE(Int)
BASE(Integer)
BASE(Float)
BASE(Double)
BASE(Int8)
BASE(Int16)
BASE(Int32)
BASE(Int64)
BASE(Word8)
BASE(Word16)
BASE(Word32)
BASE(Word64)

instance (Analyze a1 ra1, r ~ TCon1 c ra1) => Analyze (c a1)  r
instance (Analyze a1 ra1, Analyze a2 ra2, r ~ TCon2 c ra1 ra2) => Analyze (c a1 a2)  r
instance (Analyze a1 ra1, Analyze a2 ra2, Analyze a3 ra3, r ~ TCon3 c ra1 ra2 ra3) => Analyze    (c a1 a2 a3)  r
instance (Analyze a1 ra1, Analyze a2 ra2, Analyze a3 ra3, Analyze a4 ra4, r ~ TCon4 c ra1 ra2 ra3 ra4) => Analyze    (c a1 a2 a3 a4)  r
instance (Analyze a1 ra1, Analyze a2 ra2, Analyze a3 ra3, Analyze a4 ra4, Analyze a5 ra5, r ~ TCon5 c ra1 ra2 ra3 ra4 ra5) => Analyze (c a1 a2 a3 a4 a5)  r
instance (r ~ TVar a) => Analyze a r

analyze :: Analyze a b => a -> b
analyze = undefined

----------------------------------
-- Convert an analysed type to a TypeRep.
-- The tricky part is to assign a fresh type variable to each Haskell type variable.

-- Return a 'TypeRep' for a type.  Type variables look like type constructors,
-- but start with a lower case letter.
class PolyTypeable a where
   polyTypeOf :: a -> TypeRep

instance (Analyze a result, MyTypeable' HNil gout result) =>
         PolyTypeable a  where
    polyTypeOf a = fst $ mytypof' HNil (analyze a)

class MyTypeable' g0 gout classification | g0 classification -> gout where
     mytypof' :: g0 -> classification -> (TypeRep, gout)

instance (Typeable a) => MyTypeable' g0 g0 (TCon0 a) where
     mytypof' g0 _ = (typeOf (undefined::a), g0)

instance (Typeable1 c, MyTypeable' g0 g1 a1) =>
         MyTypeable' g0 g1 (TCon1 c a1) where
     mytypof' g0 _ = let (c, c_list)     = splitTyConApp $ typeOf1 (undefined :: c ())
                         (tr1,g1)   = mytypof' g0 (undefined::a1)
                     in  (mkTyConApp c (c_list ++ [tr1]), g1)

instance (Typeable2 c, MyTypeable' g0 g a1, MyTypeable' g g2 a2) =>
         MyTypeable' g0 g2 (TCon2 c a1 a2) where
     mytypof' g0 _ = let (c, c_list)     = splitTyConApp $ typeOf2 (undefined :: c () ())
                         (tr1,g1)   = mytypof' g0 (undefined::a1)
                         (tr2,g2)   = mytypof' g1 (undefined::a2)
                     in  (mkTyConApp c (c_list ++ [tr1, tr2]), g2)

instance (Typeable3 c, MyTypeable' g0 g1 a1, MyTypeable' g1 g2 a2, MyTypeable' g2 g3 a3) =>
         MyTypeable' g0 g3 (TCon3 c a1 a2 a3) where
     mytypof' g0 _ = let (c, c_list)     = splitTyConApp $ typeOf3 (undefined :: c () () ())
                         (tr1,g1)   = mytypof' g0 (undefined::a1)
                         (tr2,g2)   = mytypof' g1 (undefined::a2)
                         (tr3,g3)   = mytypof' g2 (undefined::a3)
                     in  (mkTyConApp c (c_list ++ [tr1, tr2, tr3]), g3)

instance (Typeable4 c, MyTypeable' g0 g1 a1, MyTypeable' g1 g2 a2, MyTypeable' g2 g3 a3, MyTypeable' g3 g4 a4) =>
         MyTypeable' g0 g4 (TCon4 c a1 a2 a3 a4) where
     mytypof' g0 _ = let (c, c_list)     = splitTyConApp $ typeOf4 (undefined :: c () () () ())
                         (tr1,g1)   = mytypof' g0 (undefined::a1)
                         (tr2,g2)   = mytypof' g1 (undefined::a2)
                         (tr3,g3)   = mytypof' g2 (undefined::a3)
                         (tr4,g4)   = mytypof' g3 (undefined::a4)
                     in  (mkTyConApp c (c_list ++ [tr1, tr2, tr3, tr4]), g4)

instance (Typeable5 c, MyTypeable' g0 g1 a1, MyTypeable' g1 g2 a2, MyTypeable' g2 g3 a3, MyTypeable' g3 g4 a4, MyTypeable' g4 g5 a5) =>
         MyTypeable' g0 g5 (TCon5 c a1 a2 a3 a4 a5) where
     mytypof' g0 _ = let (c, c_list) = splitTyConApp $ typeOf5 (undefined :: c () () () () ())
                         (tr1,g1)   = mytypof' g0 (undefined::a1)
                         (tr2,g2)   = mytypof' g1 (undefined::a2)
                         (tr3,g3)   = mytypof' g2 (undefined::a3)
                         (tr4,g4)   = mytypof' g3 (undefined::a4)
                         (tr5,g5)   = mytypof' g4 (undefined::a5)
                     in  (mkTyConApp c (c_list ++ [tr1, tr2, tr3, tr4, tr5]), g5)

instance (HIndex a g0 n, MyTypeable'' n g0 gout (TVar a)) =>
         MyTypeable' g0 gout (TVar a) where
     mytypof' = mytypof'' (undefined::n)


class MyTypeable'' n g0 gout classification | n g0 classification -> gout where
     mytypof'' :: n -> g0 -> classification -> (TypeRep, gout)

instance HLen g0 n1 => MyTypeable'' Z g0 (HCons a g0) (TVar a) where
     mytypof'' _ g0 _ = (mkany (undefined::S n1), HCons (undefined::a) g0)

instance Nat n => MyTypeable'' (S n) g0 g0 (TVar a) where
     mytypof'' _ g0 _ = (mkany (undefined::S n), g0)


mkany :: Nat n => n -> TypeRep
mkany n = mkTyConApp (mkTyCon ts) []
    where ts = "a" ++ show (nat n)

----------------------------------
-- Bits and pieces from HList.

-- Lookup the index of an item x in the list l
-- The index is 1-based. If not found, return 0
class Nat n => HIndex x l n | x l -> n

instance HIndex x HNil Z

instance (Nat n, TypeEq x a f, HIndex' f x (HCons a l) n)
   => HIndex x (HCons a l) n

class HIndex' f x l n | f x l -> n

instance HLen l n => HIndex' HTrue x l n
instance HIndex x l n => HIndex' HFalse x (HCons a l) n

class Nat n => HLen l n | l -> n
instance HLen HNil Z
instance HLen l n => HLen (HCons a l) (S n)

data Z
data S a

class Nat a where nat :: a -> Int
instance Nat Z where nat _ = 0
instance Nat a => Nat (S a) where nat _ = succ (nat (undefined::a))


data HTrue
data HFalse

data HNil = HNil
data HCons a b = HCons a b


class TypeEq' () x y b => TypeEq x y b | x y -> b
   where type'eq :: x -> y -> b
         type'eq _ _ = undefined::b
class TypeEq' q x y b | q x y -> b
class TypeEq'' q x y b | q x y -> b
instance TypeEq' () x y b => TypeEq x y b
instance b ~ HTrue => TypeEq' () x x b
instance TypeEq'' q x y b => TypeEq' q x y b
instance TypeEq'' () x y HFalse