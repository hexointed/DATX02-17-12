module RayUnitQC where

import Test.QuickCheck
import DFUQC
import Base
import DistFunc
import Vector
import RayUnit
import qualified Prelude as P

toListExtend :: KnownNat n => [a] -> a -> Vec n a
toListExtend l e = repl' 0 l vec
	where
		vec = repeat e
		repl' n []     v = v
		repl' n (x:xs) v = repl' (n+1) xs (replace n x v)

parse' :: String -> [Maybe FunOp]
parse' = (P.++ [Nothing]) . P.map Just .parse

testFuncs :: Vec 128 (Maybe FunOp)
testFuncs = flip toListExtend Nothing $ 
	parse' "1 2 3 + +" P.++
	parse' "x 1 -"

simRU = simulate (mealy stepR (initialize origin (position 1 0 0) testFuncs)) l'
	where
		l' = (let x = (origin, position 1 0 0) : x in x)
