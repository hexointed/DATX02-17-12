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


testdata :: [(Reset,Pack,Pack)]
testdata =
	[(Next 1 (Right(Arg 0)), repeat 0, repeat 0)]
	P.++ p'  (repeat 0) (repeat 0) (parse "p0 p0 * p1 p1 * + 1 -") P.++
	[(Next 2 (Right(Arg 0)), repeat 0, repeat 0)]
	P.++ p'  (repeat 0) (repeat 0) (parse "p0 p0 * p1 p1 * + 1 -") P.++
	[(Next 3 (Right (Arg 0)), repeat 0, repeat 0)]
	P.++ p'  (repeat 0) (repeat 0) (parse "p0 p0 * p1 p1 * + 1 -") P.++
	[(Compute (Right (Arg 0)),   repeat 0, repeat 0)]

simulate' a b = 
	putStr .
	unlines . 
	P.map show . 
	P.take (P.length b) $ 
	simulate a b

p' :: Pack -> Pack -> [FunOp] -> [(Reset, Pack, Pack)]
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
		pw str     = Right $ Point $ read str



