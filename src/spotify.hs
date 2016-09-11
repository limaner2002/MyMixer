{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}

import ClassyPrelude
import Prelude ()
import Transient.Base
import Database.Persist.Sqlite
import Control.Monad.Base

import McScraper
import Mixer
import Core (dbLocation)

data CommandOptions
    = Mc
    | Mix
    | Test
      deriving (Show, Eq, Read, Typeable)

main = keep $ do
  selectedOp <- oneThread $ option Mc "Scrape tracks from the Music Choice website."
            <|> option Mix "Take the tracks from the SQLite database and create a mix from them."
            <|> option Test "Run the test function."
  case selectedOp of
    Mc -> mcScraper
    Mix -> runSqlite dbLocation $ findTracks
    Test -> oneThread testFcn

testFcn :: (MonadIO m, MonadBase IO m) => m ()
testFcn = do
  answer <- liftBase $ keep $ input (const True) "Yes" <|> input (const True) "No"
  case answer of
    No -> putStrLn "You chose no!"
    Yes -> putStrLn "You chose yes!"


-- input :: (Read a, Show a, Typeable a, MonadBaseControl IO m) =>
--      (a -> Bool) -> String -> m a
-- input f str = do
--   r <- liftBase $ keep' $ TB.input f str
--   r' <- return r
--   _
