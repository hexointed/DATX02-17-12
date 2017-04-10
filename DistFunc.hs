{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module DistFunc where

import Float
import Pack
import Base
import Indexed

type FunOp = Either Op Data
type SPack = Vec 256 Float

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
	| Floor
	deriving (Eq, Show, Generic, NFData)

arity Sqrt = 1
arity Abs = 1
arity Floor = 1
arity _ = 2

apply :: Op -> Float -> Float -> Float
apply Max a b = max a b
apply Min a b = min a b
apply Add a b = a + b
apply Sub a b = a - b
apply Mul a b = a * b
apply Div a b = a / b
apply Sqrt a b = sqrt b
apply Abs a b = abs b
apply Floor a b = shiftL (shiftR b 16) 16


lookUp :: Float -> Pack -> Data -> Float
lookUp global local (Arg a)   = local !! a
lookUp global local (Point p) = global
