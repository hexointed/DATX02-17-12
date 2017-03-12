{-# LANGUAGE DeriveGeneric #-}

module DistFunc where

import GHC.Generics (Generic)
import Control.DeepSeq
import Float
import Base

type FunId = Unsigned 16
type FunIndex = Unsigned 8

data FunOp -- 0 000000000000000000000000000000000000000000000000000000000000000000
	= Fun Op
	| FunData Data
	deriving (Eq, Show, Generic)

data Data -- 00 0000000000000000000000000000000000000000000000000000000000000000
	= Val Float
	| X
	| Y
	| Z
	deriving (Eq, Show, Generic)

data Op -- 000
	= Max
	| Min
	| Add
	| Mul
	| Sqrt
	| Abs
	deriving (Eq, Show, Generic)

data Position = Position
	{ x :: Float
	, y :: Float
	, z :: Float
	}
	deriving (Eq, Show, Generic)

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

instance NFData FunOp
instance NFData Data
instance NFData Op
instance NFData Position
