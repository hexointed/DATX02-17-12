{-# LANGUAGE DeriveGeneric #-}

module Stack (Stack, filled, empty, push, pop, popN, top, topN) where

import Base

data Stack a = Stack (Vec 16 a) (Unsigned 4)
	deriving (Eq, Show, Generic)

filled :: a -> Stack a
filled a = Stack (repeat a) (-1)

push :: a -> Stack a -> Stack a
push a (Stack v i) = Stack (replace i' a v) i' 
	where
		i' = i + 1

top :: Stack a -> a
top (Stack v i) = v !! i

pop :: Stack a -> Stack a
pop (Stack v i) = Stack v (i - 1)

topN n (Stack v i) = v !! (i - n)
popN n (Stack v i) = Stack v (i - n)

instance NFData a => NFData (Stack a)

instance Functor Stack where
	fmap f (Stack v i) = Stack (fmap f v) i

instance Applicative Stack where
	pure v = push v empty
	(<*>) (Stack va ia) (Stack vb ib) = Stack (va <*> vb) (min ia ib)

instance Alternative Stack where
	empty = Stack (repeat undefined) (-1)
	(<|>) a@(Stack va ia) b@(Stack vb ib) 
		| ia <= ib  = a
		| otherwise = b
