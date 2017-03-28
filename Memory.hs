module Memory where

import Core

fetch :: CoreOut -> CoreIn 
fetch CO = { Nothing
           , fetchDFUInstr (dfuIPtr CO)
           , fetchDFUData (dfuDPtr CO)
           , fetchCFUInstr (cfuIPtr CO)
           }

fetchDFUInstr :: Ptr DIMem -> -> Reset
fetchDFUInstr 
