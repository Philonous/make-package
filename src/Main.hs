{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Main entry point.

module Main where

import           Data.Maybe          (catMaybes)
import           Data.Text           (Text)
import           Github
import           IO
import           Options
import           Templates

import           Control.Lens        ((^.), to)
import           Control.Monad       (when)
import           Control.Monad.Trans (liftIO)
import           Data.Monoid
import qualified Data.Text           as T
import           Data.Text.Lens
import           Data.Time
import           Data.Time.Lens
import           System.Directory

-- import System.Process

-- | Main entry point.
main :: IO ()
main = withConfig $ do
  handleArgs
  packageName <- prompt "package" "Package Name" ""
  packageDir <- optionDefault "directory" packageName
  checkExists packageDir
  author <- confOrGitOrPrompt "defaults.author" "user.name"  "Author" ""
  email <- confOrGitOrPrompt "defaults.email" "user.email" "Author Email" ""
  desc <- prompt "description" "Description" ""
  licenseType <- getLicense
  category <- stored "categories"
  now <- liftIO getCurrentTime
  let substitutions :: [(Text, Text)]
      substitutions = [("name"     ,packageName)
                      ,("desc"     ,desc)
                      ,("email"    ,email)
                      ,("author"   ,author)
                      ,("year"     ,now ^. years . to show . packed)
                      ,("license"  ,licenseType)
                      ]
                      ++ catMaybes [("category",) <$> category]
      templateConf = templates packageName substitutions
  maybeClone packageName packageDir
  liftIO $ do createDirectoryIfMissing False (T.unpack packageName)
              setCurrentDirectory (T.unpack packageName)
  runTemplates templateConf
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
      fileExists <- doesFileExist (T.unpack pname)
      dirExists <- doesDirectoryExist (T.unpack pname)
      when (fileExists || dirExists) $
        err $ "File or directory \"" <> pname <>"\" already exists."
