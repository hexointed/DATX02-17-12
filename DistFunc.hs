{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module DistFunc where

import Float
import Vector
import Pack
import Base
import Indexed

type FunOp = Either Op Data

data Data
	= Point (Ptr Pack)
	| Arg (Ptr Pack)
	deriving (Eq, Show, Generic, NFData)

data Op
	= Max
	| Min
	| Add
	| Sub
	| Mul
	| Div
	| Sqrt
	| Abs
	deriving (Eq, Show, Generic, NFData)

arity Sqrt = 1
arity Abs = 1
arity _ = 2

apply :: (Ord a, Floating a) => Op -> a -> a -> a
apply Max a b = max a b
apply Min a b = min a b
apply Add a b = a + b
apply Sub a b = a - b
apply Mul a b = a * b
apply Div a b = a / b
apply Sqrt a b = sqrt a
apply Abs a b = abs a


lookUp :: Pack -> Pack -> Data -> Float
lookUp globalp stack (Arg a) = stack !! a
lookUp globalp stack (Point pointer) = globalp !! pointer


