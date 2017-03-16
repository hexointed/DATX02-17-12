module RayUnitQC where

import RayUnit
import Test.QuickCheck
import DFUQC
import Base
import DistFunc
import Vector
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
testFuncs = flip toListExtend Nothing $ do 
	parse' "1 2 x + +"
	parse' "0 x - 7 +"
