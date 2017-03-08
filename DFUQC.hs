module DFUQC where

import SceneUnit
import DistFunc
import Base

testdata :: [Reset]
testdata = [
		Reset origin 1,
		Continue (FunData X), 
		Continue (FunData X),
		Continue (Fun Mul),
		Continue (FunData Y),
		Continue (FunData Y),
		Continue (Fun Mul),
		Continue (Fun Add),
		Continue (FunData (Val (-1))),
		Continue (Fun Add),
		Reset (Position (-3000) 2 0) 2,
		Continue (FunData X), 
		Continue (FunData X),
		Continue (Fun Mul),
		Continue (FunData Y),
		Continue (FunData Y),
		Continue (Fun Mul),
		Continue (Fun Add),
		Continue (FunData (Val (-1))),
		Continue (Fun Add),
		Reset origin 3
	]
