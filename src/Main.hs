{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point.

module Main where

import           Control.Monad
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Data.Time.Lens (getL,year)
import           Paths_make_package
import           System.Directory
import           System.IO
import           System.Process

-- | Main entry point.
main :: IO ()
main = do
  name <- prompt "Name"
  desc <- prompt "Description"
  exposed <- prompt "Exposed Module(s)"
  category <- prompt "Category(s)"
  email <- get "git" ["config","--get","user.email"]
  author <- get "git" ["config","--get","user.name"]
  time <- getCurrentTime
  createDirectoryIfMissing False (T.unpack name)
  setCurrentDirectory (T.unpack name)
  createDirectoryIfMissing False ("src/")
  let copyTemplate infile outfile =
        do template <- file infile >>= T.readFile
           T.writeFile
             outfile
             (substitute template
                         [("name",name)
                         ,("desc",desc)
                         ,("email",email)
                         ,("author",author)
                         ,("year",T.pack (show (getL year time)))
                         ,("category",category)
                         ,("exposed",exposed)])
  copyTemplate "package.cabal" (T.unpack (name <> ".cabal"))
  copyTemplate "dot-gitignore" ".gitignore"
  copyTemplate "Setup.hs" "Setup.hs"
  copyTemplate "LICENSE" "LICENSE"
  copyTemplate "README.md" "README.md"
  copyTemplate "Main.hs" "src/Main.hs"
  copyTemplate "Package.hs" (T.unpack ("src/" <> exposed <> ".hs"))
  void (get "cabal" ["configure"])
  void (get "git" ["init"])
  void (get "git" ["add","."])

  where prompt p =
          do hSetBuffering stdout NoBuffering
             putStr (p <> "> ")
             fmap (T.pack) getLine
        get prog args = fmap (T.concat . take 1 . T.lines . T.pack)
                             (readProcess prog args "")
        file fp = getDataFileName ("files/" <> fp)
        substitute = foldl (\str (this,that) -> T.replace ("$" <> this) that str)
