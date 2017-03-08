module DistFunc where

import Float
import Base

data FunOp
	= Fun Op
	| FunData Data

data Data
	= Val Float
	| X
	| Y
	| Z

data Op
	= Max
	| Min
	| Add
	| Mul
	| Sqrt
	| Abs

data Position = Position
	{ x :: Float
	, y :: Float
	, z :: Float
	}

origin :: Position
origin = Position 0 0 0

arity Sqrt = 1
arity Abs = 1
arity _ = 2

apply :: Op -> Float -> Float -> Float
apply Max a b = max a b
apply Min a b = min a b
apply Add a b = a + b
apply Mul a b = a * b
apply Sqrt a b = sqrt a
apply Abs a b = abs a
