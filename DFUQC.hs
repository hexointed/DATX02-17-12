module DFUQC where

import qualified Prelude as P
import SceneUnit
import DistFunc
import Base

testdata :: [Reset]
testdata = [
		Reset origin 1,
		Continue (Right X), 
		Continue (Right X),
		Continue (Left Mul),
		Continue (Right Y),
		Continue (Right Y),
		Continue (Left Mul),
		Continue (Left Add),
		Continue (Right (Val (-1))),
		Continue (Left Add),
		Reset (Position (-3000) 2 0) 2,
		Continue (Right X), 
		Continue (Right X),
		Continue (Left Mul),
		Continue (Right Y),
		Continue (Right Y),
		Continue (Left Mul),
		Continue (Left Add),
		Continue (Right (Val (-1))),
		Continue (Left Add),
		Reset origin 3
	]
