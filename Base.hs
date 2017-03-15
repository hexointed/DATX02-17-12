module Base	(
		module CLaSH.Prelude,
		module P,
		minWith,
		liftf
	) where

import CLaSH.Prelude hiding (Float, Double)
import qualified Prelude as P

liftf f a b = (\x -> a x `f` b x)

instance Num r => Num (a -> r) where 
	(+) = liftf (+)
	(-) = liftf (-)
	(*) = liftf (*)
	negate = (negate .)
	abs    = (abs .)
	signum = (signum .)
	fromInteger = const . fromInteger

minWith :: Ord a => (b -> a) -> b -> b -> b
minWith f a b
	| f a <= f b = a
	| otherwise  = b
