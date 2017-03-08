module Base	(
		module CLaSH.Prelude,
		module P,
		minWith
	) where

import CLaSH.Prelude hiding (Float, Double)
import qualified Prelude as P

minWith :: Ord a => (a, b) -> (a, b) -> (a, b)
minWith (a,b) (c,d)
	| a <= c    = (a, b)
	| otherwise = (c, d)
