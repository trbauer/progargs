{-# LANGUAGE ForeignFunctionInterface #-}
module Prog.Args.Native(
    getConsoleWidth
  ) where

import Foreign.C.Types

getConsoleWidth :: IO Int
-- getConsoleWidth = return 0
getConsoleWidth = fmap fromIntegral get_console_width

foreign import ccall unsafe "get_console_width" get_console_width :: IO CInt

