module DFUQC where

import DFU
import DistFunc
import Base

testdata :: [Either FunOp Position]
testdata = [
		Left (FunData X), 
		Left (FunData X),
		Left (Fun Mul),
		Left (FunData Y),
		Left (FunData Y),
		Left (Fun Mul),
		Left (Fun Add),
		Left (FunData (Val (-1))),
		Left (Fun Add),
		Right origin
	]
