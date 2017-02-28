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

test2 = do 
	fn <- curryN 2
	return [FunD (mkName "curry") [Clause [] (NormalB fn) []]]

test3 = $(_claAddN 4)
