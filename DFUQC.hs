module DFUQC where

import qualified Prelude as P
import DFU
import DistFunc
import Base

testdata :: [(Reset,Position)]
testdata = [
		(Next 1, Position 0 0 0 ),
		(Continue (Right X), Position 0 0 0 ),
		(Continue (Right X), Position 0 0 0 ),
		(Continue (Left Mul), Position 0 0 0 ),
		(Continue (Right Y), Position 0 0 0 ),
		(Continue (Right Y), Position 0 0 0 ),
		(Continue (Left Mul), Position 0 0 0 ),
		(Continue (Left Add), Position 0 0 0 ),
		(Continue (Right (Val (-1))), Position 0 0 0 ),
		(Continue (Left Add), Position 0 0 0 ),
		(Next 2, Position 0 0 0 ),
		(Continue (Right X), Position 0 0 0 ),
		(Continue (Right X), Position 0 0 0 ),
		(Continue (Left Mul), Position 0 0 0 ),
		(Continue (Right Y), Position 0 0 0 ),
		(Continue (Right Y), Position 0 0 0 ),
		(Continue (Left Mul), Position 0 0 0 ),
		(Continue (Left Add), Position 0 0 0 ),
		(Continue (Right (Val (-1))), Position 0 0 0 ),
		(Continue (Left Add), Position 0 0 0 ),
		(Next 3, Position 0 0 0 ),
		(Done, Position 0 0 0)
	]

simulate' a b = 
	putStr .
	unlines . 
	P.map show . 
	P.take (P.length b) $ 
	simulate a b
