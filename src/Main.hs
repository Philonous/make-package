{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Main entry point.

module Main where

import           Data.Maybe          (catMaybes)
import           Data.Text           (Text)
import           Github
import           IO
import           Options
import           Stackage
import           System.FilePath     ((</>))
import           Templates

import           Control.Lens        ((^.), to)
import           Control.Monad       (when)
import           Control.Monad.Trans (liftIO)
import           Data.Monoid
import qualified Data.Text           as T
import           Data.Text.Lens
import           Data.Time
import           Data.Time.Lens
import qualified System.Directory    as Dir

-- import System.Process

-- | Main entry point.
main :: IO ()
main = withConfig $ do
  handleArgs
  packageName <- prompt "package" "Package Name" ""
  packageDir <- optionDefault "directory" packageName
  checkExists packageDir
  author <- confOrGitOrPromptString "defaults.author" "user.name"  "Author" ""
  email <- confOrGitOrPromptString "defaults.email" "user.email" "Author Email" ""
  desc <- prompt "description" "Description" ""
  licenseType <- getLicense
  category <- stored "categories"
  resolver <- getResolver
  now <- liftIO getCurrentTime
  pwd <- liftIO Dir.getCurrentDirectory
  let substitutions :: [(Text, Text)]
      substitutions = [("name"     ,packageName)
                      ,("desc"     ,desc)
                      ,("email"    ,email)
                      ,("author"   ,author)
                      ,("year"     ,now ^. years . to show . packed)
                      ,("license"  ,licenseType)
                      ,("stack-resolver", resolver)
                      ]
                      ++ catMaybes [("category",) <$> category]
  maybeClone packageName packageDir
  liftIO $ Dir.createDirectoryIfMissing False (T.unpack packageDir)
  mbTemplate <- stored "template.name"
  let templateConf = templates packageName substitutions (T.unpack packageDir)
  runTemplate templateConf mbTemplate
  withCurrentDirectory (T.unpack packageDir) $ do
    unlessConf "git.enable" (== False) $ do
      run "git" ["init"]
      run "git" ["add", "."]
      whenConf "git.do-commit" (== True) $ do
        msg <- confLookupDefault "git.initial-commit-message" "initial commit"
        run "git" ["commit", "-m", msg]

    handleGithub packageName desc
    run "stack" ["build", "--install-ghc", "--test", "--no-run-tests"]
  where
    checkExists pname = liftIO $ do
      fileExists <- Dir.doesFileExist (T.unpack pname)
      dirExists <- Dir.doesDirectoryExist (T.unpack pname)
      when (fileExists || dirExists) $
        err $ "File or directory \"" <> pname <>"\" already exists."
