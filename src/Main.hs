{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

-- | Main entry point.

module Main where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as S
import           Data.Char (isSpace)
import           Data.List (isPrefixOf, isSuffixOf)
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as E
import           Data.Time
import           Data.Time.Lens
import           Paths_make_package
import           System.Console.Haskeline as HL
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

import IO

-- import System.Process

-- | Main entry point.
main :: IO ()
main = withConfig $ do
  packageName <- prompt "Package Name"
  desc <- prompt "Description"
  licenseType <- getLicense
  exposed <- prompt "Exposed Module(s)"
  category <- prompt "Category(s)"
  author <- confOrGitOrPrompt "author" "user.name"  "Author"
  email <- confOrGitOrPrompt "email" "user.email" "Author Email"
  time <- liftIO getCurrentTime
  liftIO $ do createDirectoryIfMissing False (T.unpack packageName)
              setCurrentDirectory (T.unpack packageName)
              createDirectoryIfMissing False ("src/")
  let substitutions = [("name"     ,packageName)
                      ,("desc"     ,desc)
                      ,("email"    ,email)
                      ,("author"   ,author)
                      ,("year"     ,T.pack (show (getL year time)))
                      ,("category" ,category)
                      ,("exposed"  ,exposed)
                      ,("license"  ,licenseType)]
      copyTemplate = doCopyTemplate substitutions
  copyTemplate "package.cabal" (T.unpack (packageName <> ".cabal"))
  copyTemplate "dot-gitignore" ".gitignore"
  copyTemplate "Setup.hs" "Setup.hs"
  copyTemplate ("licenses"</> T.unpack licenseType <.> "license") "LICENSE"
  copyTemplate "README.md" "README.md"
  copyTemplate "Main.hs" "src/Main.hs"
  copyTemplate "Package.hs" (T.unpack ("src/" <> exposed <> ".hs"))
  run "cabal" ["configure"]
  unlessConf "use-git" (== False) $ do
    run "git" ["init"]
    run "git" ["add", "."]
    whenConf "git-do-commit" (== True) $ do
      run "git" ["commit", "-m","initial commit"]
  where
    doCopyTemplate substitutions infile outfile = liftIO $
        do template <- dataFile infile >>= T.readFile
           T.writeFile outfile (substitute template substitutions)
    substitute = foldl (\str (this,that) -> T.replace ("$" <> this) that str)
