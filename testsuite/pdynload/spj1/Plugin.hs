module Plugin where

-- doesn't import the API and provides a polymorphic value
-- should pass type check, and dump core

resource :: Num a => a
resource = 7
