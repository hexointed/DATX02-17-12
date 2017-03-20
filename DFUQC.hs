module DFUQC where

import qualified Prelude as P
import DFU
import DistFunc
import Base
import Vector

testdata :: [(Reset,Position)]
testdata =
	[(Next 1, position 0 0 0 )]
	P.++ p' (position 2 3 0) (parse "x x * y y * + 1 -") P.++
	[(Next 2, position 0 0 0 )]
	P.++ p' (position 1 2 0) (parse "x x * y y * + 1 -") P.++
	[(Next 3, position 0 0 0 )]
	P.++ p' (position 5 7 0) (parse "x x * y y * + 1 -") P.++
	[(Done, position 0 0 0)]

simulate' a b = 
	putStr .
	unlines . 
	P.map show . 
	P.take (P.length b) $ 
	simulate a b

p' :: Position -> [FunOp] -> [(Reset, Position)]
p' p l = flip P.zip (let x = p : x in x) (P.map Continue l)

parse :: String -> [FunOp]
parse str = P.map pw $ words str
	where
		pw "+" = Left Add
		pw "-" = Left Sub
		pw "*" = Left Mul
		pw "/" = Left Div
		pw "x" = Right X
		pw "y" = Right Y
		pw "z" = Right Z
		pw "M" = Left Max
		pw "m" = Left Min
		pw str = Right $ Val $ read str
