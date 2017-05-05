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
topEntity = mealy (\sum inp -> (\x -> (x,x)) $ sqrtApprox inp + sum) 0 . register 0

-- square root on 16 16 signed fixed numbers
-- 24 iterations on 48 internal bits
sqrtFS :: Float -> Float
sqrtFS x = 
	bitCoerce 
	(resize# (iSqrt48 $ expand x) ::Unsigned 32)
	where
		expand :: Fixed Signed 16 16 -> Unsigned 48
		expand x = bitCoerce (resizeF x :: SFixed 16 32)

isNegative :: KnownNat n => Unsigned (n + 1) -> Bool
isNegative x = not $ bitCoerce $ head $ vec x
	where
		vec :: (KnownNat n) => Unsigned n -> Vec n Bit
		vec x = bitCoerce x

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

loopStepN n (num, res) = (temp `asTypeOf` num , res' `asTypeOf` num)
	where
		(neg, temp) = subHighBits n num (nextRes n'' res True)
		resShift = res `shiftR` 1
		n' = mulSNat n d2
		n'' = subSNat n' d1
		res' = nextRes n'' resShift neg

nextRes :: (KnownNat n, KnownNat m) => 
	SNat n -> Unsigned (n + m + 1) -> Bool -> Unsigned (n + m + 1)
nextRes n' r c = unpack (pack (highBits n' r) ++# pack c ++# 0)

subHighBits n a b = (,) neg $ unpack 
		((pack (if neg then diff else ah)) ++# (pack $ lowBits n' a))
	where
		n' = mulSNat n d2
	
		ah = highBits n' a
		bh = highBits n' b
	
		ah' = (unpack $ 0 ++# (pack $ lowBits n ah)) `asTypeOf` ah
		bh' = (unpack $ 0 ++# (pack $ lowBits n bh)) `asTypeOf` bh
	
		diff = ah' - bh'
		neg = isNegative diff

highBits :: (KnownNat n, KnownNat m) => SNat n -> Unsigned (n + m) -> Unsigned n
highBits n = unpack . bTake n . pack

lowBits :: (KnownNat n, KnownNat m) => SNat n -> Unsigned (n + m) -> Unsigned m
lowBits n u = resize u

sqrtApprox :: Float -> Float
sqrtApprox f = bitCoerce . resize . froot . unpack $
	(0 :: BitVector 16) ++# pack f ++# (0 :: BitVector 16)

froot :: Unsigned 64 -> Unsigned 64
froot v = rl + ((xor v l) * (rh - rl)) `shiftR` shift
	where
		h = l `shiftL` 1
		l = roundPow2 v
		rl = fastroot l
		rh = fastroot h
		shift = bitCoerce (resize (ilog l)) :: Int

roundPow2 :: KnownNat n => Unsigned n -> Unsigned n
roundPow2 v = v'' . snd $ mapAccumL (\a x -> (a .|. x, if a==high then 0 else x)) low (v' v)
	where 
		v' :: KnownNat n => Unsigned n -> Vec n Bit
		v' = bitCoerce

		v'' :: KnownNat n => Vec n Bit -> Unsigned n
		v'' = bitCoerce

fastroot :: KnownNat n => Unsigned (n + n) -> Unsigned (n + n)
fastroot v = unpack (0 ++# pack root)
	where
		root = evens $
			v .|.
			v `shiftL` 1 .|.
			v `shiftR` 2

evens :: KnownNat n => Unsigned (2 * n) -> Unsigned n
evens = bitCoerce . map head . unconcat d2 . v'
	where 
		v' :: KnownNat n => Unsigned n -> Vec n Bit
		v' = bitCoerce

ilog v =
	snd .
	logStepN d1 .
	logStepN d2 .
	logStepN d4 .
	logStepN d8 .
	logStepN d16 .
	logStepN d32 $
	(v, 0 `asTypeOf` v)

logStepN n (v, r) =
	if v .&. b n == 0
	then (v, r)
	else (v `shiftR` s' n, r .|. s n)

b :: (KnownNat n, KnownNat k) => SNat n -> Unsigned (k + n + n)
b n = unpack (
		0 ++# 
		pack (ones n) ++# 
		pack (0 `asTypeOf` ones n)
	)
	where
		ones :: KnownNat n => SNat n -> Unsigned n
		ones n = (-1) 

s :: (KnownNat n, KnownNat m) => SNat n -> Unsigned m
s = fromInteger . natVal

s' :: KnownNat n => SNat n -> Int
s' = fromInteger . natVal
