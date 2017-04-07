module DFUQC where

import qualified Prelude as P
import DFU
import DistFunc
import Base
import Pack
import Stateful

toListExtend :: KnownNat n => [a] -> a -> Vec n a
toListExtend l e = repl' 0 l vec
	where
		vec = repeat e
		repl' n []     v = v
		repl' n (x:xs) v = repl' (n+1) xs (replace n x v)

parse' :: String -> [Maybe FunOp]
parse' = (P.++ [Nothing]) . P.map Just . parse

testFuncs :: Vec 128 (Maybe FunOp)
testFuncs = flip toListExtend Nothing $ 
	parse' "p0 p1 -" P.++
	parse' "p1 5 + p0 - p0 3 / p1 + 15 - M" P.++
	parse' "p0 30 - p0 30 - * p1 10 - p1 10 - * + 10 -"


testdata :: [(Reset,SPack,Pack)]
testdata =
	[(Next 1, repeat 0, repeat 0)]
	P.++ p'  (repeat 2) (repeat 6) (parse "s0 p1 * p1 s1 * / s1 +") P.++
	[(Next 2, repeat 0, repeat 0)]
	P.++ p'  (repeat 10) (repeat 5) (parse "p0 p0 * p1 p1 * + s1 m") P.++
	[(Next 3, repeat 0, repeat 0)]
	P.++ p'  (repeat 5) (repeat 1) (parse "p0 p0 * p1 p1 * + s1 M") P.++
	[(Compute,   repeat 0, repeat 0)]

simulate' a b = 
	putStr .
	unlines . 
	P.map show . 
	P.take (P.length b) $ 
	simulate a b

p' :: SPack -> Pack -> [FunOp] -> [(Reset, SPack, Pack)]
p' p p1 l = P.zip3 (P.map Continue l) (let x = p : x in x) (let x = p1 : x in x)

parse :: String -> [FunOp]
parse str = P.map pw $ words str
	where
		pw "+" = Left Add
		pw "-" = Left Sub
		pw "*" = Left Mul
		pw "/" = Left Div
		pw "M" = Left Max
		pw "m" = Left Min
		pw ('p':n) = Right $ Arg $ read n
		pw ('s':n) = Right $ Point $ read n



