{-# LANGUAGE ScopedTypeVariables #-}
module StackQC where

import Stack
import CLaSH.Prelude
import Test.QuickCheck
import qualified Prelude as P

instance Arbitrary a => Arbitrary (Stack a) where
	arbitrary = do
		index <- arbitrary :: Gen a
		vals <- arbitrary :: Gen [a]
		return $ P.foldr push empty vals

prop_push_top :: [Int] -> Stack Int -> Property
prop_push_top xs s = P.length xs <= 16 ==> unwind (P.length xs) full == xs
	where
		full = P.foldr push empty xs
		unwind n s
			| n > 0     = top s : unwind (n - 1) (pop s)
			| otherwise = []

prop_push_topN :: [Int] -> Stack Int -> Property
prop_push_topN xs s = P.length xs <= 16 ==> 
	and $ P.zipWith (==) (P.map (flip topN full) [0 ..]) (xs)
	where
		full = P.foldr push empty xs
