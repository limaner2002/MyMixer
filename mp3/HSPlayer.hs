module HSPlayer
    where

import Foreign
import Foreign.C
import Foreign.C.String

foreign import ccall "loadLocalFile" c_loadLocalFile :: Ptr CChar -> Int

loadLocalFile :: String -> IO ()
loadLocalFile path = do
  c_path <- newCString path
  let result = c_loadLocalFile c_path
  print result
