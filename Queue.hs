{-# LANGUAGE DeriveGeneric, DeriveAnyClass, 
	ScopedTypeVariables, FlexibleInstances #-}

module Queue (Queue, module Container, popN, topN) where

import Base hiding (Index)
import Container
import Indexed

type Index n = Unsigned (Log 2 n)
type Pow2 n = (FLog 2 n ~ CLog 2 n, KnownNat n)

type Queue = Q
data Q n a = Q (Vec n a) (Index n) (Index n) (Unsigned 1)
	deriving (Eq, Show, Generic, NFData)

instance Pow2 n => Functor (Queue n) where
	fmap g (Q v h t f) = Q (fmap g v) h t f

instance Pow2 n => Applicative (Queue n) where
	pure v = push v empty
	Q va ha ta fa <*> Q vb hb tb fb = Q (va <*> vb) h' t' (fa .&. fb)
		where
			h' = (min ha hb)
			t' = (max ta tb)

instance Pow2 n => Alternative (Queue n) where
	empty = filled undefined
	Q va ha ta fa <|> Q vb hb tb fb
		| ha - ta <= hb - tb = Q va ha ta fa
		| otherwise          = Q vb hb tb fb

instance Pow2 n => Container (Queue n) where
	filled a = Q (repeat a) 0 0 1
	
	push a (Q v h t f) = Q (replace h a v) h' t f'
		where
			f' = bitWith (fold (.&.)) $ complement $ xor h' t
			h' :: Unsigned n
			h' = h + 1
	
	pop (Q v h t f) = Q v h (t + 1) 0
	
	top (Q v h t f) = v !! t

popN :: Pow2 n => Unsigned (Log 2 n) -> Q n a -> Q n a
popN n (Q v h t f) = Q v h (t + n) 0

topN :: Pow2 n => Unsigned (Log 2 n) -> Q n a -> a
topN n (Q v h t f) = v !! (t + n)

instance Pow2 n => Indexed (Queue n a) where
	type Size (Queue n a) = Log 2 n
