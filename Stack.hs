{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Stack (Stack, module Container, popN, topN) where

import Base
import Container
import Indexed

data Stack n a = Stack (Vec n a) (Unsigned (CLog 2 n))
	deriving (Eq, Generic, NFData)

type Pow2 n = (FLog 2 n ~ CLog 2 n, KnownNat n)

instance Functor (Stack n) where
	fmap f (Stack v i) = Stack (fmap f v) i

instance Pow2 n => Applicative (Stack n) where
	pure v = push v empty
	(<*>) (Stack va ia) (Stack vb ib) = Stack (va <*> vb) (min ia ib)

instance Pow2 n => Alternative (Stack n) where
	empty = filled undefined
	(<|>) a@(Stack va ia) b@(Stack vb ib) 
		| ia <= ib  = a
		| otherwise = b

instance Pow2 n => Container (Stack n) where
	filled a = Stack (repeat a) (-1)

	push a (Stack v i) = Stack (replace i' a v) i' 
		where
			i' = i + 1

	pop (Stack v i) = Stack v (i - 1)

	top (Stack v i) = v !! i

topN n (Stack v i) = v !! (i - n)
popN n (Stack v i) = Stack v (i - n)

instance (Show a, Pow2 n) => Show (Stack n a) where
	show (Stack v i) = show $ take (i + 1) list
		where
			list = foldr (:) [] v
			take 0 xs     = []
			take n (x:xs) = x : take (n - 1) xs

instance Indexed (Stack n a) where
	type Size (Stack n a) = CLog 2 n
