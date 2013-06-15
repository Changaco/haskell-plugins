{-# OPTIONS -cpp #-}

#include "../../../../config.h"

import Plugins
import API

main = do
        m_v <- dynload "../Plugin.o" ["../api"]
                                     ["../../../../plugins.conf.inplace"]
                                     "resource_dyn"
        case m_v of
                LoadFailure _ -> error "didn't compile"
                LoadSuccess _ (Interface eq) -> do
                                 putStrLn $ show $   1 `eq` 2
                                 putStrLn $ show $ 'a' `eq` 'b'

