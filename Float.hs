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

topEntity :: Signal (Unsigned 32) -> Signal (Unsigned 32)
topEntity = mealy (\sum inp -> (\x -> (x,x)) $ ilog inp + sum) 0 . register 0

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

--froot v = rl' + (v' - l') * (rh' - rl') / (h' - l')
--	where
--		h = roundPow2 v
--		l = h `shiftR` 1
--		rl = fastroot l
--		rh = fastroot h
--
--		h' = uf d16 (resize h `shiftL` 16 :: Unsigned 48)
--		l' = uf d16 (resize l `shiftL` 16 :: Unsigned 48)
--		rl' = uf d16 (resize rl `shiftL` 16 :: Unsigned 48)
--		rh' = uf d16 (resize rh `shiftL` 16 :: Unsigned 48)
--		v' = uf d16 (resize v `shiftL` 16 :: Unsigned 48)

roundPow2 v = (+1) $ 
	v - 1 .|.
	v `shiftR` 1 .|.
	v `shiftR` 2 .|.
	v `shiftR` 4 .|.
	v `shiftR` 8 .|.
	v `shiftR` 16 

fastroot :: KnownNat n => Unsigned (n * 2) -> Vec n Bit
fastroot v = smap bit (unconcat d2 (v' v))
	where
		v' :: KnownNat n => Unsigned n -> Vec n Bit
		v' v = bitCoerce v

		bit n _ = at (mulSNat d2 n) (v' v)

ilog v =
	snd .
	logStepN d1 .
	logStepN d2 .
	logStepN d4 .
	logStepN d8 .
	logStepN d16 $
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
