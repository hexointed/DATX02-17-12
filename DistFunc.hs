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
	| Addv
	| Subv
	| Scale
	| Copy
	deriving (Eq, Show, Generic, NFData)

arity Sqrt = 1
arity Abs = 1
arity Floor = 1
arity Dot = 6
arity Cross = 6
arity Addv = 6
arity Subv = 6
arity Scale = 4
arity Copy = 1
arity _ = 2

apply :: Op -> Float -> Float -> Float -> Float -> Float -> Float -> Either Float (Float,Float,Float)
apply Max   a b _ _ _ _ = Left (max a b)
apply Min   a b _ _ _ _ = Left (min a b)
apply Add   a b _ _ _ _ = Left (a + b)
apply Sub   a b _ _ _ _ = Left (b - a)
apply Mul   a b _ _ _ _ = Left (a * b)
apply Div   a b _ _ _ _ = Left (b / a)
apply Sqrt  a b _ _ _ _ = Left (sqrt a)
apply Abs   a b _ _ _ _ = Left (abs a)
apply Floor a b _ _ _ _ = Left (shiftL (shiftR a 16) 16)
apply Dot   a1 a2 a3 b1 b2 b3 = Left (a1*b1 + a2*b2 + a3*b3)
apply Cross a1 a2 a3 b1 b2 b3 = Right (((a2*b3-a3*b2),(a3*b1-a1*b3),(a1*b2-b1*a2)))
apply Addv a1 a2 a3 b1 b2 b3 = Right ((a1+b1),(a2+b2),(a3+b3))
apply Subv a1 a2 a3 b1 b2 b3 = Right ((a1-b1),(a2-b2),(a3-b3))
apply Scale s a1 a2 a3 _  _ = Right ((s*a1), (s*a2),(s*a3))
apply Copy c _ _ _ _ _ = Left c
