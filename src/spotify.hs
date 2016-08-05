{-# LANGUAGE OverloadedStrings #-}

import Prelude ()
import ClassyPrelude
import Flow

main :: IO ()
main = do
  (uri:_) <- getArgs

  let expr = Get $ FlowAtom $ parseURI uri

  r <- tryAny $ execute Nothing expr

  case showResult r of
    Left exc -> print exc
    Right v -> print v

showResult :: Either SomeException (Flow, Either SomeException SpotifyItem) -> Either SomeException Text
showResult r = do
  (_, eItem) <- r
  item <- eItem
  return $ tshow item