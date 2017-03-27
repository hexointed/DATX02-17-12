module Stateful where

import CLaSH.Prelude

data State a
	= Result a -- A result is available
	| Ready    -- Ready to start a new batch
	| WaitI    -- Ready for next instruction

class Stateful a where
	type Out a :: *
	type In a :: *

	step :: a -> Input a -> (a, Output a)
	initial :: a

	nextState :: a -> Input a -> a
	nextState a i = fst $ step a i

	output :: a -> Input a -> Output a
	output a i = snd $ step a i

type Output a = State (Out a)
type Input a = In a
