module API where

import Data.Typeable

data TestIO = TestIO { field :: IO String }

instance Typeable TestIO where
#if __GLASGOW_HASKELL__ >= 603
    typeOf i = mkTyConApp (mkTyCon "API.TestIO") []
#else
    typeOf i = mkAppTy (mkTyCon "API.TestIO") []
#endif

testio :: TestIO
testio = TestIO $ return "default value"
