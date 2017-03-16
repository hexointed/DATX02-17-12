{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module DistFunc where

import Float
import Base

type FunId = Unsigned 16
type FunIndex = Unsigned 8
type FunOp = Either Op Data

data Data
	= Val Float
	| X
	| Y
	| Z
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

data Position = Position
	{ x :: Float
	, y :: Float
	, z :: Float
	}
	deriving (Eq, Show, Generic, NFData)

origin :: Position
origin = Position 0 0 0

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

lookUp :: Position -> Data -> Float
lookUp p (Val v) = v
lookUp p X       = x p
lookUp p Y       = y p
lookUp p Z       = z p
