{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module Float where

import CLaSH.Sized.Internal.Unsigned
import Base hiding (pack)
import CLaSH.Prelude (pack)

type GenFloat (a :: Nat) = Fixed Signed a a
type Float = GenFloat 16
type Point = Unsigned 4

instance Floating (GenFloat 16) where
	sqrt = sqrtFS
	pi = undefined
	exp = undefined
	log = undefined
	sin = undefined
	cos = undefined
	asin = undefined
	acos = undefined
	atan = undefined
	sinh = undefined
	cosh = undefined
	asinh = undefined
	acosh = undefined
	atanh = undefined

topEntity :: Signal Float -> Signal Float
topEntity = mealy (\sum inp -> (\x -> (x,x)) $ sqrtFS inp + sum) 0 . register 0

-- square root on 16 16 signed fixed numbers
-- 24 iterations on 48 internal bits
sqrtFS :: Float -> Float
sqrtFS x = 
	bitCoerce 
	(resize# (iSqrt48 $ expand x) ::Unsigned 32)
	where
		expand :: Fixed Signed 16 16 -> Unsigned 48
		expand x = bitCoerce (resizeF x :: SFixed 16 32) :: Unsigned 48

isNegative :: KnownNat n => Unsigned (n + 1) -> Bool
isNegative x = (x `and#` (1 `shiftL` fromInteger (natVal $ y x))) == 0
	where
		y = undefined :: KnownNat n => Unsigned (n + 1) -> Unsigned n
	
iSqrt48 :: Unsigned 48 -> Unsigned 48
iSqrt48 x =
	snd .
	loopStepN d24 .
	loopStepN d23 .
	loopStepN d22 .
	loopStepN d21 .
	loopStepN d20 .
	loopStepN d19 .
	loopStepN d18 .
	loopStepN d17 .
	loopStepN d16 .
	loopStepN d15 .
	loopStepN d14 .
	loopStepN d13 .
	loopStepN d12 .
	loopStepN d11 .
	loopStepN d10 .
	loopStepN d9 .
	loopStepN d8 .
	loopStepN d7 .
	loopStepN d6 .
	loopStepN d5 .
	loopStepN d4 .
	loopStepN d3 .
	loopStepN d2 .
	loopStepN d1 $
	(x ,0) -- these are the starting values for num, res

loopStepN n (num, res) =
	( if cond then temp else num
	, res'
	)
	where
		temp = subHighBits n num (nextRes n'' res True)
		cond = isNegative temp
		resShift = res `shiftR` 1
		n' = mulSNat n d2
		n'' = subSNat n' d1
		res' = nextRes n'' resShift cond

nextRes :: (KnownNat n, KnownNat m) => 
	SNat n -> Unsigned (n + m + 1) -> Bool -> Unsigned (n + m + 1)
nextRes n' r c = unpack (pack# (highBits n' r) ++# pack c ++# 0)

subHighBits n a b = unpack $
	(++#)
	(pack (ah' - bh'))
	(pack $ lowBits n' a)
	where
		n' = mulSNat n d2

		ah = highBits n' a
		bh = highBits n' b

		ah' = (unpack $ 0 ++# (pack $ lowBits n ah)) `asTypeOf` ah
		bh' = (unpack $ 0 ++# (pack $ lowBits n bh)) `asTypeOf` bh

highBits :: (KnownNat n, KnownNat m) => SNat n -> Unsigned (n + m) -> Unsigned n
highBits n = unpack . bTake n . pack#

lowBits :: (KnownNat n, KnownNat m) => SNat n -> Unsigned (n + m) -> Unsigned m
lowBits n u = resize u
