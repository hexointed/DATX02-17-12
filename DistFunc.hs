{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module DistFunc where

import Float
import Pack
import Base
import Indexed

type SPack = Vec 256 Float

data FunOp
	= Point (Ptr SPack)
	| Arg (Ptr Pack)
	| Oper Op
	| Acc
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
	| Nop
	deriving (Eq, Show, Generic, NFData)

arity Sqrt = 1
arity Abs = 1
arity Floor = 1
arity Nop = 1
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
apply Nop a b = b
