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

-- import System.Process

-- | Main entry point.
main :: IO ()
main = do
  name <- prompt "Name"
  desc <- prompt "Description"
  licenseType <- promptLicense
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
                         ,("exposed",exposed)
                         ,("license",licenseType)])
  copyTemplate "package.cabal" (T.unpack (name <> ".cabal"))
  copyTemplate "dot-gitignore" ".gitignore"
  copyTemplate "Setup.hs" "Setup.hs"
  copyTemplate ("licenses"</> T.unpack licenseType<.>"license") "LICENSE"
  copyTemplate "README.md" "README.md"
  copyTemplate "Main.hs" "src/Main.hs"
  copyTemplate "Package.hs" (T.unpack ("src/" <> exposed <> ".hs"))
  run "cabal" ["configure"]
  run "git" ["init"]
  run "git" ["add","."]

  where
    prompt p = runInputT defaultSettings $ do
        ln <- HL.getInputLine (p <> "> ")
        case ln of
            Nothing -> liftIO $ exitFailure
            Just line -> return $ T.pack line
    promptLicense = do
        licensesDir <- file "licenses"
        files <- map takeBaseName . filter (".license" `isSuffixOf` ) <$>
                        getDirectoryContents licensesDir
        T.pack <$> choose "license" files
    get prog args = fmap (T.concat . take 1 . T.lines . T.pack)
                         (readProcess prog args "")
    run pg args = rawSystem pg args >>= \case
        ExitSuccess -> return ()
        ExitFailure n -> do
                        putStrLn $ "Program " ++ show pg
                            ++ " exited with error code " ++ show n
                        exitFailure
    file fp = getDataFileName ("files/" <> fp)
    substitute = foldl (\str (this,that) -> T.replace ("$" <> this) that str)


-- | Let the user choose a string from a list
choose :: String -> [String] -> IO String
choose p xs = do
    runInputT (setComplete cf defaultSettings) (HL.outputStrLn options >> go)
  where
    pairs = zip (map show [1..]) xs
    options = unlines $ map (\(l,r) -> l <> ") " <> r ) pairs
    cf = completeWord Nothing " "
           (\wd -> return $ simpleCompletion <$> filter (wd `isPrefixOf`) xs)
    go = do
        mbln <- HL.getInputLine (p <> "> ")
        ln <- case mbln of
            Nothing -> liftIO exitFailure
            Just ln -> return . strip $ ln
        if | ln `elem` xs -> return ln
           | Just line <- ln `lookup` pairs -> return line
           | "?" <- ln -> HL.outputStrLn options >> go
           | otherwise -> do
               HL.outputStrLn $ ln ++ " is not valid, type ? to show options"
               go
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
