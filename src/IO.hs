{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module IO where

import           Control.Applicative ((<$>))
import           Control.Monad.Reader
import           Data.Char (isSpace)
import qualified Data.Configurator as Conf
import qualified Data.Configurator.Types as Conf
import           Data.List (isPrefixOf, isSuffixOf)
import           Data.Monoid
import qualified Data.Text as T
import           System.Console.Haskeline as HL
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process

import           Paths_make_package

newtype MakePackage a = MP {unConf :: ReaderT Conf.Config IO a}
                        deriving (Monad, MonadIO, Functor, MonadException)

withConfig :: MakePackage a -> IO a
withConfig (MP f) = do conf <- loadConfig
                       runReaderT f conf


getConf :: MakePackage Conf.Config
getConf = MP ask

confLookup :: Conf.Configured a => Conf.Name -> MakePackage (Maybe a)
confLookup c = getConf >>= liftIO . (`Conf.lookup` c)


-- | Prompt user
prompt :: T.Text -> MakePackage T.Text
prompt p = runInputT defaultSettings $ do
    ln <- HL.getInputLine (T.unpack $ p <> "> ")
    case ln of
        Nothing -> liftIO $ exitFailure
        Just line -> return $ T.pack line

-- | Get option from configuration file or prompt user
confOrPrompt :: T.Text -> MakePackage T.Text
confOrPrompt p = confLookup p >>= \case
    Nothing -> prompt p
    Just x  -> return x

-- | Get option from configuration file, git configuration or promp user
confOrGitOrPrompt :: T.Text -> T.Text -> T.Text -> MakePackage T.Text
confOrGitOrPrompt c gitQ p = confLookup c >>= \case
    Just x -> return x
    Nothing -> confLookup "use-git" >>= \case
               Just True -> queryGit (T.unpack gitQ) >>= \case
                   Just x -> return x
                   Nothing -> prompt p
               _ -> prompt p

-- | Get license from configuration or prompt
getLicense :: MakePackage T.Text
getLicense = do
    licensesDir <- liftIO $ dataFile "licenses"
    licenses <- map takeBaseName . filter (".license" `isSuffixOf` ) <$>
                    liftIO (getDirectoryContents licensesDir)
    defaultLicense <- confLookup "default-license" >>= \case
        Nothing -> return Nothing
        Just l -> if (l `elem` licenses)
                  then return . Just $ T.pack l
                  else do liftIO . putStrLn $
                              "Warning, configured default license \""
                              ++ l ++ "\" is unknown"
                          return Nothing
    case defaultLicense of
        Just l -> return l
        Nothing -> T.pack <$> selectFrom "license" licenses

-- | Query git configuration
queryGit :: String -> MakePackage (Maybe T.Text)
queryGit q = liftIO (readProcessWithExitCode "git" ["config","--get", q] "")
             >>= \case
    (ExitSuccess, res, _stderr) -> return . Just $ T.pack res
    _ -> return Nothing
  where
    get :: FilePath -> [String] -> MakePackage T.Text
    get prog args = liftIO $ fmap (T.concat . take 1 . T.lines . T.pack)
                         (readProcess prog args "")

-- | Run a programm, exit with error when program returns failure code
run :: String -> [String] -> MakePackage ()
run pg args = liftIO $ rawSystem pg args >>= \case
    ExitSuccess -> return ()
    ExitFailure n -> do
                    putStrLn $ "Program " ++ show pg
                        ++ " exited with error code " ++ show n
                    exitFailure

-- | Get the file path of packaged file
dataFile :: String -> IO String
dataFile fp = getDataFileName ("files/" <> fp)

-- | Load the configuration files
loadConfig :: IO Conf.Config
loadConfig = do
    appData <- getAppUserDataDirectory "make-package"
    home <- getHomeDirectory
    Conf.load [ Conf.Optional $ appData </> "config"
              , Conf.Optional $ home </> ".make-package"
              ]

-- | Run action unless configuration option is set and predicate is true
unlessConf :: Conf.Configured a => Conf.Name
                                -> (a -> Bool)
                                -> MakePackage ()
                                -> MakePackage ()
unlessConf option p f = confLookup option >>= \case
    Just x | p x -> return ()
    _ -> f

-- | Run action when configuration option is set and predicate is true
whenConf :: Conf.Configured a => Conf.Name
                              -> (a -> Bool)
                              -> MakePackage ()
                              -> MakePackage ()
whenConf option p f = confLookup option >>= \case
    Just x | p x -> f
    _ -> return ()

-- | Let the user select a string from a list
selectFrom :: String -> [String] -> MakePackage String
selectFrom p xs = liftIO $ do
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
