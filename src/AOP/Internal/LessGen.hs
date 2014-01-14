{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses,
             TypeFamilies,
             FunctionalDependencies,
             UndecidableInstances,
             FlexibleInstances,
             OverlappingInstances,
             IncoherentInstances
 #-}

module AOP.Internal.LessGen (LessGen, LeastGen, Analyze, LeastGen', SubstEmpty) where

import AOP.Internal.PolyTypeable

-- Check if two analysed types can be matchedd.
-- The tricky part is to construct a substitution using type classes.

-- | If LeastGen a b c holds then there exists a substitution s and type c such that (s c = (a, b)).
class LeastGen a b c

-- LeastGen is defined in terms of LeastGen'. We discard the constructed substitution sout.
instance (Analyze a _a, Analyze b _b, LeastGen' _a _b c SubstEmpty sout) => LeastGen a b c

-- | If LessGen a b, then b is less general than a. That is, there exists substitution s such that (s b = a).
class LessGen a b

-- | It unifies a with b after checking the substitution s.
-- | We use this to delay unification in the following instance declaration for LessGen.
-- | If we just use typical unification, this will be done *before* constructing/checking the substitution.
-- | The idea is that if the substitution exists, then the unification is one-way and is performed.
class Unifies s a b
instance a ~ b => Unifies SubstEmpty a b
instance a ~ b => Unifies (SubstCons x y s) a b

 -- LessGen is also defined in terms of LeastGen'
instance (Analyze a _a, Analyze b _b, LeastGen' _a _b b SubstEmpty sout, Unifies sout a b) => LessGen a b


-- LeastGen' extends the substitution s0 to sout 
-- such that (sout b = a).
class (Substitution sin, Substitution sout) => LeastGen' a b c sin sout | sin a b c -> sout

-- Constant types we check if they are both the same
instance (Substitution s0, a ~ c) => LeastGen' (TCon0 a) (TCon0 a) c s0 s0

-- for n-ary constructors, the substitution is extended for each argument
-- and chained from left to right 
instance (LeastGen' a1 b1 c1 s0 s1, 
          d ~ d', d c1 ~ c)
       => LeastGen' (TCon1 (d ()) a1) (TCon1 (d' ()) b1) c s0 s1

instance (LeastGen' a1 b1 c1 s0 s1,
          LeastGen' a2 b2 c2 s1 s2,
          d ~ d', d c1 c2 ~ c)
       => LeastGen' (TCon2 (d () () ) a1 a2) (TCon2 (d' () ()) b1 b2) c s0 s2

instance (LeastGen' a1 b1 c1 s0 s1,
          LeastGen' a2 b2 c2 s1 s2,
          LeastGen' a3 b3 c3 s2 s3,
          d ~ d', d c1 c2 c3 ~ c)
       => LeastGen' (TCon3 (d () () ()) a1 a2 a3) (TCon3 (d' () () ()) b1 b2 b3) c s0 s3

instance (LeastGen' a1 b1 c1 s0 s1,
          LeastGen' a2 b2 c2 s1 s2,
          LeastGen' a3 b3 c3 s2 s3,
          LeastGen' a4 b4 c4 s3 s4,
          d ~ d', d c1 c2 c3 c4 ~ c)
       => LeastGen' (TCon4 (d () () () ()) a1 a2 a3 a4) (TCon4 (d' () () () ()) b1 b2 b3 b4) c s0 s4

instance (LeastGen' a1 b1 c1 s0 s1,
          LeastGen' a2 b2 c2 s1 s2,
          LeastGen' a3 b3 c3 s2 s3,
          LeastGen' a4 b4 c4 s3 s4,
          LeastGen' a5 b5 c5 s4 s5,
          d ~ d', d c1 c2 c3 c4 c5 ~ c)
       => LeastGen' (TCon5 (d () () () () ()) a1 a2 a3 a4 a5) (TCon5 (d' () () () () ()) b1 b2 b3 b4 b5) c s0 s5

-- default case when a and b doesn't share the same constructor
instance (Substitution sin, Substitution sout,
          MapsTo sin (a, b) c',
          VarCase c' (a, b) c sin sout,
          Analyze (W c) (TVar c)
 )       => LeastGen' a b c sin sout

-- extends the substitution if required
class (MaybeType v, Substitution sin, Substitution sout) => VarCase v ab c sin sout | v ab sin -> sout c
instance Substitution sin => VarCase None ab c sin (SubstCons ab c sin)
instance Substitution sin => VarCase (Some c) ab c sin sin -- check that s(b) = a

----------------------------------

-- | Encoding of substitutions as partial maps 

data SubstEmpty
data SubstCons x sx s

class Substitution s 
instance Substitution SubstEmpty
instance Substitution s => Substitution (SubstCons x sx s)
 
data None
data Some a

class MaybeType a
instance MaybeType None
instance MaybeType (Some a)

-- | Examines substitution s and binds sx the variable that maps to x in s, or None.
class (Substitution s, MaybeType sx) => MapsTo s x sx | s x -> sx

instance MapsTo SubstEmpty x None
instance Substitution s => MapsTo (SubstCons x sx s) x (Some sx) 
instance (Substitution s, MapsTo s x sx) => MapsTo (SubstCons x' sx' s) x sx





-- | Plotkin's example of the bit of iron

-- To make Analyze work on I419, could be made more pretty using TemplateHaskell
-- instance (r ~ TCon0 I419)  => Analyze I419 r
instance (r ~ TCon0 I419)  => Analyze I419 r; instance (r ~ TCon0 I419)  => Analyze (W I419) r

instance (r ~ TCon0 Bit1)  => Analyze Bit1 r; instance (r ~ TCon0 Bit1)  => Analyze (W Bit1) r
instance (r ~ TCon0 Bit2)  => Analyze Bit2 r; instance (r ~ TCon0 Bit2)  => Analyze (W Bit2) r


data Bitofiron a = Bitofiron a
data Heated a b  = Heated a b
data Melted a    = Melted a

data Bit1
data Bit2 
data I419

bit1 :: Bitofiron Bit1 -> Heated Bit1 I419 -> Melted Bit1
bit1 = undefined

bit2 :: Bitofiron Bit2 -> Heated Bit2 I419 -> Melted Bit2
bit2 = undefined

generalize :: LeastGen t1 t2 t3 => t1 -> t2 -> t3
generalize = undefined

genBit = generalize bit1 bit2
