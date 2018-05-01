{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Stackage where

import           Control.Lens
import qualified Control.Monad.Catch as Ex
import           Control.Monad.Trans
import           Data.Aeson.Lens
import           Data.Text           (Text)
import           Network.Wreq        as Wreq
import           System.IO

import           IO

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
    Just lts -> return lts
    Nothing ->
      liftIO getLatestLts >>= \case
        Nothing -> do
          return "lts-11.7"
        Just lts -> do
          liftIO . hPutStrLn stderr $
            "Choosing latest stackage lts: " ++ (show lts)
          return lts
