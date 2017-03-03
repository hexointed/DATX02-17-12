{-# LANGUAGE TemplateHaskell #-}

module Tmp where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad
import Prelude ((++), ($), (.), div, (-), show)
import qualified Prelude as P
import CLaSH.Prelude hiding ((++))
import qualified CLaSH.Prelude ((++))
