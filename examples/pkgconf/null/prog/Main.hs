{-# OPTIONS -cpp #-}

#include "../../../../config.h"

import Plugins
import API

main = do
    let includes = TOP ++ "/examples/load/null/api"
    (_,v) <- load "../Null.o" ["."] ["../api/package.conf"] "resource"
    putStrLn ( show (a v) )

