module Plugin where

import API
import Data.Dynamic

v :: Int
v = 0xdeadbeef

resource_dyn :: Dynamic
resource_dyn = toDyn v

