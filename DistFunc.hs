{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module DistFunc where

import Float
import Vector
import Pack
import Base
import Indexed

type FunOp = Either Op Data
type SPack = Vec 32 Float

data Data
	= Point (Ptr SPack)
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


lookUp :: SPack -> Pack -> Data -> Float
lookUp global local (Arg a) = local !! a
lookUp global local (Point pointer) = global !! pointer


