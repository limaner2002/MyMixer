{-# LANGUAGE OverloadedStrings #-}
module Keychain where

import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.ForeignPtr.Safe
import qualified Data.ByteString.Char8 as BS
import System.IO

foreign import ccall "StorePasswordKeychain" c_StorePasswordKeychain :: Ptr CChar -> Int -> Ptr CChar ->
                                                                        Int-> Ptr CChar -> Int -> Int
foreign import ccall "GetPasswordKeychain" c_GetPasswordKeychain :: Ptr a -> CUInt -> Ptr a ->
                                                                    CUInt -> CString
foreign import ccall "&ffree" ffree :: FunPtr (CString -> IO())

saveRefreshToken :: String -> String -> Maybe BS.ByteString -> IO ()
saveRefreshToken _ _ Nothing = BS.hPutStrLn stderr "No refresh token to save!"
saveRefreshToken service account (Just pass) = do
  cService <- newCStringLen service
  cAccount <- newCStringLen account
  cPass <- newCStringLen $ BS.unpack pass

  newForeignPtr ffree (fst cService)
  newForeignPtr ffree (fst cAccount)
  newForeignPtr ffree (fst cPass)

  let result = c_StorePasswordKeychain svc svclen acct acctlen passwd passlen
          where
            (passwd, passlen) = cPass
            (svc, svclen) = cService
            (acct, acctlen) = cAccount

  putStrLn $ show result

fromKeychain :: String -> String -> IO (Maybe String)
fromKeychain service account = do
  cService <- newCStringLen service
  cAccount <- newCStringLen account

  newForeignPtr ffree (fst cService)
  newForeignPtr ffree (fst cAccount)

  let result = c_GetPasswordKeychain svc (fromIntegral svclen) acct (fromIntegral acctlen)
          where
            (svc, svclen) = cService
            (acct, acctlen) = cAccount
  newForeignPtr ffree result

  -- Convert from c-space to haskell space
  checkResult result

checkResult :: CString -> IO (Maybe String)
checkResult c_str
    | c_str == nullPtr = return Nothing
    | otherwise = do
                  result <- peekCString c_str
                  return $ Just result
