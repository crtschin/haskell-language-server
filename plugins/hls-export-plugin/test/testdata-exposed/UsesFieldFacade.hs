module UsesFieldFacade where

import           FieldFacade (fieldUsed)

consumedField :: Int
consumedField = fieldUsed undefined
