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
	| Dot
	| Cross
	deriving (Eq, Show, Generic, NFData)

arity Sqrt = 1
arity Abs = 1
arity Floor = 1
arity _ = 2

apply :: Op -> Float -> Float -> Float -> Float -> Float -> Float -> Float
apply Max a b _ _ _ _  = max a b
apply Min a b _ _ _ _ = min a b
apply Add a b _ _ _ _ = a + b
apply Sub a b _ _ _ _ = a - b
apply Mul a b _ _ _ _ = a * b
apply Div a b _ _ _ _ = a / b
apply Sqrt a b _ _ _ _ = sqrt b
apply Abs a b _ _ _ _ = abs b
apply Floor a b _ _ _ _  = shiftL (shiftR b 16) 16
apply Dot a1 a2 a3 b1 b2 b3 = a1*b1 + a2*b2 + a3*b3
apply Cross a1 a2 a3 b1 b2 b3 = 
