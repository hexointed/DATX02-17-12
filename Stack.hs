{-# LANGUAGE DeriveGeneric #-}

module Stack (Stack, filled, empty, push, pop, popN, top, topN) where

import GHC.Generics (Generic)
import Control.DeepSeq
import DistFunc
import Float
import Base

data Stack a = Stack
	{ vec :: Vec 16 a
	, index :: Unsigned 4
	}
	deriving (Eq, Show, Generic)

filled :: a -> Stack a
filled a = Stack (repeat a) (-1)

push :: a -> Stack a -> Stack a
push v stack = stack {
		vec = replace index' v (vec stack),
		index = index'
	}
	where
		index' = index stack + 1

top :: Stack a -> a
top s = vec s !! index s

pop :: Stack a -> Stack a
pop s = s { index = index s - 1 }

topN i s = vec s !! (index s - i)
popN i s = s { index = index s - i }

instance NFData a => NFData (Stack a)

instance Functor Stack where
	fmap f s = s { vec = fmap f (vec s) }

instance Applicative Stack where
	pure v = push v empty
	(<*>) a b = Stack (vec a <*> vec b) (min (index a) (index b))

instance Alternative Stack where
	empty = Stack (repeat undefined) (-1)
	(<|>) a b 
		| index a <= index b = a
		| otherwise          = b
