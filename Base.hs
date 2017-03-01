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

test3 = $(_claAddN 1)
