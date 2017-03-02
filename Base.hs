{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Base where

import CLaSH.Prelude
import Language.Haskell.TH
import Control.Monad
import Rec

or' = (.|.)
and' = (.&.)
xor' = xor
not' = complement

$(insN 
		32 
		_claAddN 
		"test" 
		(\t -> [t| $(t) -> $(t) -> Bit -> ( $(t), Bit, Bit ) |] )
	)
