{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point.

module Main where

import           IO
import           Github

import           Control.Monad.Trans (liftIO)
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Data.Time.Lens
import           System.Directory
import           System.FilePath


-- import System.Process

-- | Main entry point.
main :: IO ()
main = withConfig $ do
  packageName <- prompt "Package Name"
  maybeClone packageName
  author <- confOrGitOrPrompt "defaults.author" "user.name"  "Author"
  email <- confOrGitOrPrompt "defaults.email" "user.email" "Author Email"
  desc <- prompt "Description"
  licenseType <- getLicense
  exposed <- prompt "Exposed Module(s)"
  category <- prompt "Category(s)"
  now <- liftIO getCurrentTime
  liftIO $ do createDirectoryIfMissing False (T.unpack packageName)
              setCurrentDirectory (T.unpack packageName)
              createDirectoryIfMissing False ("src/")
  let substitutions = [("name"     ,packageName)
                      ,("desc"     ,desc)
                      ,("email"    ,email)
                      ,("author"   ,author)
                      ,("year"     ,T.pack (show (getL year now)))
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
  unlessConf "git.enable" (== False) $ do
    run "git" ["init"]
    run "git" ["add", "."]
    whenConf "git.do-commit" (== True) $ do
      run "git" ["commit", "-m","initial commit"]
  handleGithub packageName desc
  where
    doCopyTemplate substitutions infile outfile = liftIO $
        do template <- dataFile infile >>= T.readFile
           T.writeFile outfile (substitute template substitutions)
    substitute = foldl (\str (this,that) -> T.replace ("$" <> this) that str)
