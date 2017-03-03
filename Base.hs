{-# LANGUAGE TemplateHaskell #-}

module Base where

import CLaSH.Prelude hiding (snat)

or' = (.|.)
and' = (.&.)
xor' = xor
not' = complement

fullAdder a b c = (carry, sum)
	where
		i = xor a b
		sum = xor i c
		carry = (a .&. b) .|. (i .&. c)

