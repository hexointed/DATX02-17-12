module Stateful where

import CLaSH.Prelude

class Stateful a where
	type Output a :: *
	type Input a :: *

	step :: a -> Input a -> (a, Output a)
	initial :: a

	nextState :: a -> Input a -> a
	nextState a i = fst $ step a i

	output :: a -> Input a -> Output a
	output a i = snd $ step a i
