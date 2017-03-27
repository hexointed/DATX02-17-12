{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Stack (Stack, module Container, popN, topN) where

import Base
import Container
import Indexed

data Stack a = Stack (Vec 16 a) (Unsigned 4)
	deriving (Eq, Generic, NFData)

instance Functor Stack where
	fmap f (Stack v i) = Stack (fmap f v) i

instance Applicative Stack where
	pure v = push v empty
	(<*>) (Stack va ia) (Stack vb ib) = Stack (va <*> vb) (min ia ib)

instance Alternative Stack where
	empty = filled undefined
	(<|>) a@(Stack va ia) b@(Stack vb ib) 
		| ia <= ib  = a
		| otherwise = b

instance Container Stack where
	filled a = Stack (repeat a) (-1)

	push a (Stack v i) = Stack (replace i' a v) i' 
		where
			i' = i + 1

	pop (Stack v i) = Stack v (i - 1)

	top (Stack v i) = v !! i

topN n (Stack v i) = v !! (i - n)
popN n (Stack v i) = Stack v (i - n)

instance Show a => Show (Stack a) where
	show (Stack v i) = show $ take (i + 1) list
		where
			list = foldr (:) [] v
			take 0 xs     = []
			take n (x:xs) = x : take (n - 1) xs

instance Indexed (Stack a) where
	type Size (Stack a) = 4
