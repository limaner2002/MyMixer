{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)
import Control.Monad (void)
import Data.Conduit
import Data.Monoid
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as CL (consume)
import qualified Data.Conduit.Combinators as CM
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Control.Monad.IO.Class
import Data.Bits
import Data.Binary
import Data.Char
import System.Environment

-- Make sure to remove this soon
import HSPlayer

type ID3Data = BS.ByteString

data FrameHeader = FrameHeader
    { frameID :: ID3Data,
      frameSize :: Word32,
      frameFlags :: ID3Data,
      val :: ID3Data
    } | Unsupported ID3Data

instance Show FrameHeader
    where
      -- show (FrameHeader id size flags val) =
      --     "frameId: " ++ C8.unpack id
      --                 ++ " size: " ++ show size
      --                 ++ " flags: " ++ (show $ BS.unpack flags)
      --                 ++ " value: " ++ C8.unpack val
      show (FrameHeader id _ _ value) =
          C8.unpack id ++ " " ++ C8.unpack value
      show (Unsupported name) =
          C8.unpack name ++ " Unsupported"

data TagHeader = TagHeader
    { tagType :: ID3Data,
      majorVersion :: ID3Data,
      minorVersion :: ID3Data,
      flags :: ID3Data,
      size :: Word32
    }

data ExtendedHeader = ExtendedHeader
    { extendedHeaderSize :: ID3Data,
      extendedNumFlagBytes :: ID3Data,
      extendedFlags :: ID3Data
    }

data ID3Tag = ID3Tag
    { header :: TagHeader,
      frames :: [FrameHeader]
    }

instance Show TagHeader
    where
      show (TagHeader tagType major minor flags size) =
          "tagType: " ++ C8.unpack tagType
                      ++ "v2." ++ (show $ BS.head major)
                      ++ "." ++ (show $ BS.head minor)
                      ++ " flags: " ++ show (BS.unpack flags)
                      ++ " size: " ++ show size

getField :: (MonadResource m) => Int -> ConduitM C8.ByteString c m C8.ByteString
getField n = do
  name <- CB.isolate n =$ CL.consume
  return $ BS.concat name

parseTagHeader :: (MonadResource m) => Sink BS.ByteString m TagHeader
parseTagHeader = do
  tagType <- getField 3
  major <- getField 1
  minor <- getField 1
  flags <- getField 1
  size <- getField 4

  return $ TagHeader tagType major minor flags $ unSyncField size

findTagHeader :: (MonadResource m) => Conduit BS.ByteString m BS.ByteString
findTagHeader = CM.map $ f
    where
      f str = snd $ BS.breakSubstring prefix str
      prefix = C8.pack "ID3"

findFrameHeader :: (MonadResource m) => Conduit BS.ByteString m BS.ByteString
findFrameHeader = CM.map $ BS.dropWhile (\x -> isAscii (chr . fromEnum $ x) == False)
    where
      go count word
         | count == 4 = []
         | isAscii word = go (count+1) word

parseFrame :: (MonadResource m) => Consumer BS.ByteString m (Maybe FrameHeader)
parseFrame = do
  frameId <- getField 4
  if C8.unpack frameId == "PRIV"
  then do
    frameSize <- getField 4
    CB.drop ((fromEnum (decode (BL.fromStrict frameSize) :: Word32))+2)
    return $ Just $ Unsupported frameId
  else do
    if C8.all (\x -> (isAlpha x) || (isDigit x)) frameId
    then do
      frameSize <- getField 4
      frameFlags <- getField 2
      let size = unSyncField frameSize
      val <- getField (fromEnum size)

      return $ Just $ FrameHeader frameId size frameFlags val
    else return Nothing


unSyncInteger :: Word32 -> Maybe Word32
unSyncInteger n
  | check n = Just $ (n .&. 0x7f)
                .|.(n .&. 0x7f00)     `shiftR` 1
                .|.(n .&. 0x7f0000)   `shiftR` 2
                .|.(n .&. 0x7f000000) `shiftR` 3
  | otherwise = Nothing
    where
      -- The 7th bit of each byte MUST be equal to zero for this
      -- syncsafe integer to be valid. See section 6.2 of the
      -- id3v2.4.0 structure document.
      check x = (0x80 .&. x)
                .|. (0x8000 .&. x)
                .|. (0x80000 .&. x)
                .|. (0x800000 .&. x) == 0

unSyncField :: BS.ByteString -> Word32
unSyncField val = do
  let result = unSyncInteger (decode (BL.fromStrict val))
  case result of
    Nothing -> error $ "There was an error unsyncing the field with value "
               ++ show (BS.unpack val)
    Just num -> num

skipPadding :: (MonadResource m) => Conduit BS.ByteString m BS.ByteString
skipPadding = CM.map $ BS.dropWhile (\x -> x == 0x00)

ignore :: (MonadResource m) => Int -> Conduit BS.ByteString m BS.ByteString
ignore n = CM.map $ BS.drop n

getFrames :: (MonadResource m) => Conduit BS.ByteString m FrameHeader
getFrames = do
  yieldWhileJust parseFrame

fun :: (MonadResource m) => FilePath -> m ()
fun path = do
  (stream, t) <- CB.sourceFile path
                 $$+ parseTagHeader
  liftIO $ putStrLn $ show t
  stream $$+- CB.isolate (fromEnum $ size t) =$ getFrames =$ CM.mapM_ (liftIO . print)
  
yieldWhileJust :: Monad m => ConduitM a b m (Maybe b) -> Conduit a m b
yieldWhileJust consumer =
    loop
        where
          loop = do
            mx <- consumer
            case mx of
              Nothing -> return ()
              Just x -> yield x >> loop

main = do
  paths <- getArgs
  mapM_ (\path -> do
           runResourceT $ fun path
           loadLocalFile path
        ) paths
