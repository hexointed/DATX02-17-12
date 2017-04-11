module Base	(
		module CLaSH.Prelude,
		module P,
		module GHC.Generics,
		module Control.DeepSeq,
		minWith,
		liftf,
		small,
		medium,
		large,
		dup,
		dup3,
		bitWith,
		BitEq,
		bTake,
		bDrop,
		bSlice,
		curry3,
		uncurry3,
		choice
	) where

import CLaSH.Prelude hiding (Float, Double, pack)
import qualified Prelude as P
import GHC.Generics (Generic)
import Control.DeepSeq
import Test.QuickCheck hiding (resize)

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

small  = Args Nothing  1000 100  200 True
medium = Args Nothing  2000 100 1000 True
large  = Args Nothing 10000 100 1000 True

dup a = (a, a)
dup3 a = (a, a, a)

type BitEq a b = (BitSize a ~ BitSize b, BitPack a, BitPack b)

bitWith :: (BitEq a b, BitEq c a) => (b -> c) -> a -> a
bitWith f = bitCoerce . f . bitCoerce

bTake :: (KnownNat n, KnownNat m) => SNat n -> BitVector (n + m) -> BitVector n
bTake n b = resize (shiftR b $ fromInteger $ natVal m)
	where m = bDrop n b

bDrop :: (KnownNat n, KnownNat m) => SNat n -> BitVector (n + m) -> BitVector m
bDrop n b = resize b

bSlice n m = bDrop n . bTake (addSNat m n)

curry3 f (a, b, c) = f a b c
uncurry3 f a b c = f (a, b, c)

choice f cs = fold (\l r -> fmap f l <*> r) cs
