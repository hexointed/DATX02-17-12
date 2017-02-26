{-# LANGUAGE TemplateHaskell #-}

module Base where

import CLaSH.Prelude
import Language.Haskell.TH
import Control.Monad
import Rec

or' = (.|.)
and' = (.&.)
xor' = xor
not' = complement

$(do 
	fn <- curryN 2
	return [FunD (mkName "curry") [Clause [] (NormalB fn) []]]
	)
