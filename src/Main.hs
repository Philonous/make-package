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
import qualified Data.Configurator as Conf
import qualified Data.Configurator.Types as Conf
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
  config <- loadConfig
  packageName <- prompt "Package Name"
  desc <- prompt "Description"
  licenseType <- getLicense config
  exposed <- prompt "Exposed Module(s)"
  category <- prompt "Category(s)"
  author <- confOrGitorPrompt config "user.name" "author" "Author"
  email <- confOrGitorPrompt config "user.email" "email" "Author Email"
  time <- getCurrentTime
  createDirectoryIfMissing False (T.unpack packageName)
  setCurrentDirectory (T.unpack packageName)
  createDirectoryIfMissing False ("src/")
  let copyTemplate infile outfile =
        do template <- file infile >>= T.readFile
           T.writeFile
             outfile
             (substitute template
                         [("name",packageName)
                         ,("desc",desc)
                         ,("email",email)
                         ,("author",author)
                         ,("year",T.pack (show (getL year time)))
                         ,("category",category)
                         ,("exposed",exposed)
                         ,("license",licenseType)])
  copyTemplate "package.cabal" (T.unpack (packageName <> ".cabal"))
  copyTemplate "dot-gitignore" ".gitignore"
  copyTemplate "Setup.hs" "Setup.hs"
  copyTemplate ("licenses"</> T.unpack licenseType <.> "license") "LICENSE"
  copyTemplate "README.md" "README.md"
  copyTemplate "Main.hs" "src/Main.hs"
  copyTemplate "Package.hs" (T.unpack ("src/" <> exposed <> ".hs"))
  run "cabal" ["configure"]
  unlessConf config "use-git" (== False) $ do
    run "git" ["init"]
    run "git" ["add","."]
    whenConf config "git-do-commit" (== True) $ do
      run "git" ["commit", "-m","initial commit"]
  where
    confOrPrompt :: Conf.Config -> T.Text -> IO T.Text
    confOrPrompt conf p = Conf.lookup conf p >>= \case
        Nothing -> prompt p
        Just x  -> return x
    confOrGitorPrompt conf gitQ c p = Conf.lookup conf c >>= \case
        Just x -> return x
        Nothing -> Conf.lookup conf "use-git" >>= \case
                   Just True -> queryGit gitQ >>= \case
                       Just x -> return x
                       Nothing -> prompt p
                   _ -> prompt p
    prompt p = runInputT defaultSettings $ do
        ln <- HL.getInputLine (T.unpack $ p <> "> ")
        case ln of
            Nothing -> liftIO $ exitFailure
            Just line -> return $ T.pack line
    getLicense conf = do
        licensesDir <- file "licenses"
        licenses <- map takeBaseName . filter (".license" `isSuffixOf` ) <$>
                        getDirectoryContents licensesDir
        defaultLicense <- Conf.lookup conf "default-license" >>= \case
            Nothing -> return Nothing
            Just l -> if (l `elem` licenses)
                      then return . Just $ T.pack l
                      else do putStrLn $ "Warning, configured default license \""
                                       ++ l ++ "\" is unknown"
                              return Nothing
        case defaultLicense of
            Just l -> return l
            Nothing -> T.pack <$> choose "license" licenses
    get prog args = fmap (T.concat . take 1 . T.lines . T.pack)
                         (readProcess prog args "")
    queryGit q = readProcessWithExitCode "git" ["config","--get", q] "" >>= \case
        (ExitSuccess, res, _stderr) -> return . Just $ T.pack res
        _ -> return Nothing

    run pg args = rawSystem pg args >>= \case
        ExitSuccess -> return ()
        ExitFailure n -> do
                        putStrLn $ "Program " ++ show pg
                            ++ " exited with error code " ++ show n
                        exitFailure
    file fp = getDataFileName ("files/" <> fp)
    substitute = foldl (\str (this,that) -> T.replace ("$" <> this) that str)
    loadConfig = do
        appData <- getAppUserDataDirectory "make-package"
        home <- getHomeDirectory
        Conf.load [ Conf.Optional $ appData </> "config"
                  , Conf.Optional $ home </> ".make-package"
                  ]
    unlessConf conf option p f = Conf.lookup conf option >>= \case
        Just x | p x -> return ()
        _ -> f
    whenConf conf option p f = Conf.lookup conf option >>= \case
        Just x | p x -> f
        _ -> return ()

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
