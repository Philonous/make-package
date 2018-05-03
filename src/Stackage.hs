{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Stackage where

import           Control.Applicative  ((<|>))
import           Control.Lens
import qualified Control.Monad.Catch  as Ex
import           Control.Monad.Trans
import qualified Data.Aeson           as Aeson
import           Data.Aeson.Lens
import           Data.Attoparsec.Text as AP
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HMap
import qualified Data.List            as List
import           Data.Monoid
import           Data.Ord             (comparing)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector
import           Network.Wreq         as Wreq

import           IO

data Snapshot
  = Other Text -- Could not parse
  | Nightly Text
  | Lts Int Int -- Major Minor
  deriving (Show, Eq, Ord)

parseSnapshot :: Text -> Snapshot
parseSnapshot txt =
  either (const $ Other txt) id $
    parseOnly (parseLts <|> parseNightly) txt
  where
    parseLts = do
      _ <- "lts-"
      major <- decimal
      _ <- "."
      minor <- decimal
      return $ Lts major minor
    parseNightly = do
      _ <- "nightly-"
      Nightly <$> takeText


getSnapshots :: IO [Text]
getSnapshots = do
  info "Getting list of recent stackage snapshots"
  Ex.try (getWith opts "https://www.stackage.org/download/snapshots.json") >>= \case
    Left (e :: Ex.SomeException) -> do
      err "Couldn't retrieve latest stackage lts"
      return mempty
    Right response -> do
      case (Aeson.eitherDecode (response ^. responseBody)) of
        Left e -> do
          err $ "Error decoding stackage snapshots: " <> (Text.pack e)
          return mempty
        Right r ->
          return
            . reverse . List.nub . List.sortBy (comparing parseSnapshot)
            $ HMap.elems @Text r
  where
    opts = Wreq.defaults & header "Accept" .~ ["application/json"]

selectSnapshot :: MonadIO m => m Text
selectSnapshot = do
  snaps <- liftIO $ getSnapshots
  Text.pack <$> selectFrom "Snapshot" (Text.unpack <$> snaps) False


-- | Get latest lts
getLatestLts :: IO (Maybe Text)
getLatestLts = do
  Ex.try (getWith opts "https://www.stackage.org/lts") >>= \case
    Left (e :: Ex.SomeException) -> do
      err "Couldn't retrieve latest stackage lts"
      return Nothing
    Right response ->
      return $ response ^? responseBody . key "snapshot" . key "name" . _String
  where
    opts = Wreq.defaults & header "Accept" .~ ["application/json"]

getResolver :: MakePackage Text
getResolver =
  stored "stack.resolver" >>= \case
    Just "lts" -> do
      info "Getting latest lts from stackage..."
      liftIO getLatestLts >>= \case
        Nothing -> do
          return "lts-11.7"
        Just lts -> do
          info $ "Choosing latest stackage lts: " <> lts
          return lts
    Just lts -> return lts
    Nothing -> selectSnapshot
