{-# LANGUAGE RankNTypes #-}

-- a really nasty type:

module API where

import Data.Generics

data Interface = Interface { field :: Typeable r => (r -> Bool) -> GenericQ [r] }

rsrc :: Interface
rsrc = Interface { field = listify }

