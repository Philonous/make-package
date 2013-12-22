{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point.

module Main where

import qualified Data.ByteString.Lazy as S
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as E
import           Data.Text.Template
import           Data.Time
import           Data.Time.Lens
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
  cabalTemplate <- file "package.cabal" >>= T.readFile
  file "dot-gitignore" >>= flip copyFile ".gitignore"
  file "LICENSE" >>= flip copyFile "LICENSE"
  file "Setup.hs" >>= flip copyFile "Setup.hs"
  file "Main.hs" >>= flip copyFile "src/Main.hs"
  -- The `template' library inserts random newlines if the variable is
  -- at the end of a line.
  S.writeFile
    (T.unpack (name <> ".cabal"))
    (E.encodeUtf8
       (unmunge
          (substitute cabalTemplate
                      (context [("name",name)
                               ,("desc",desc)
                               ,("email",email)
                               ,("author",author)
                               ,("year",T.pack (show (getL year time)))
                               ,("category",category)
                               ,("exposed",exposed)]))))
  get "cabal" ["configure"]
  return ()

  where prompt p = do hSetBuffering stdout NoBuffering
                      putStr (p <> "> ")
                      T.getLine
        get prog args = fmap T.pack (readProcess prog args "")
        file fp = getDataFileName ("files/" <> fp)
        unmunge = L.replace "\nexecutable" "\n\nexecutable"
                . L.replace "\nlibrary" "\n\nlibrary"
                . L.replace "\n\n" "\n"

-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Context
context assocs x = maybe err id . lookup x $ assocs
  where err = error $ "Could not find key: " ++ T.unpack x
