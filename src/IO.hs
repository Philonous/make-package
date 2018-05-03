{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module IO where

import qualified Control.Monad.Catch                     as Ex
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char                               (isSpace)
import qualified Data.Configurator                       as Conf
import qualified Data.Configurator.Types                 as Conf
import           Data.List                               (isPrefixOf, isSuffixOf)
import           Data.Map                                (Map)
import qualified Data.Map                                as Map
import           Data.Maybe                              (fromMaybe)
import           Data.Monoid
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import qualified Data.Text.IO                            as Text
import           System.Console.Haskeline                as HL
import           System.Console.Haskeline.MonadException ()
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

import           Paths_make_package

newtype MakePackage a = MP
  { unConf :: StateT (Map Text Conf.Value) (ReaderT Conf.Config IO) a
  } deriving (Monad, MonadIO, Functor, Applicative
             , Ex.MonadThrow, Ex.MonadCatch, Ex.MonadMask)

info :: MonadIO m => Text -> m ()
info msg = liftIO $ Text.hPutStrLn stderr $ "Info: " <> msg

warn :: MonadIO m => Text -> m ()
warn msg = liftIO $ Text.hPutStrLn stderr $ "Warning: " <> msg

err :: MonadIO m => Text -> m ()
err msg = liftIO $ do
  Text.hPutStrLn stderr $ "Error: " <> msg
  exitFailure

failure :: MonadIO m => Text -> m b
failure msg = do
  err msg
  liftIO $ exitFailure

withConfig :: MakePackage a -> IO a
withConfig (MP f) = do conf <- loadConfig
                       fst <$> runReaderT (runStateT f Map.empty) conf


stored :: Conf.Configured a =>
          Text
       -> MakePackage (Maybe a)
stored c = (Conf.convert <=< Map.lookup c) <$> MP get

optionDefault :: Conf.Configured b => Text -> b -> MakePackage b
optionDefault o d = fromMaybe d <$> stored o

confLookup :: Conf.Configured a => Conf.Name -> MakePackage (Maybe a)
confLookup c = stored c >>= \case
    Just x -> return $ Just x
    Nothing -> liftIO . (`Conf.lookup` c) =<< MP ask

confLookupDefault :: Conf.Configured a => Conf.Name -> a -> MakePackage a
confLookupDefault c d = fromMaybe d <$> confLookup c

-- | Prompt user
prompt :: Text.Text -> Text.Text -> Text -> MakePackage Text.Text
prompt c p il = stored c >>= \case
    Just x -> return x
    Nothing ->
      do ln <- liftIO $ runInputT defaultSettings $
                   HL.getInputLineWithInitial (Text.unpack $ p <> "> ")
                                              (Text.unpack il, "")
         case ln of
             Nothing -> liftIO exitFailure
             Just line ->
                 do setOption c (Conf.String $ Text.pack line)
                    return (Text.pack line)

promptYesNo :: Text.Text -> Text.Text -> MakePackage Bool
promptYesNo c p = stored c >>= \case
    Just x -> return x
    Nothing -> do liftIO $ Text.putStrLn p
                  res <- liftIO $ runInputT defaultSettings go
                  setOption c $ Conf.Bool res
                  return res
  where
    go = do char <- getInputChar "[y]yes or [n]o> "
            case char of
                Nothing -> liftIO exitFailure
                Just 'y' -> return True
                Just 'Y' -> return True
                Just 'n' -> return False
                Just 'N' -> return False
                _   -> go


setOption :: Text -> Conf.Value -> MakePackage ()
setOption c x = MP $ modify (Map.insert c x)

-- | Get option from configuration file or prompt user
confOrPrompt :: Text.Text -> Text.Text -> Text.Text -> MakePackage Text.Text
confOrPrompt c p i = confLookup c >>= \case
    Nothing -> prompt c p i
    Just x  -> return x

-- | Get option from configuration file, git configuration or promp user
confOrGitOrPromptString ::
     Text.Text -> Text.Text -> Text.Text -> Text.Text -> MakePackage Text.Text
confOrGitOrPromptString c gitQ p i = confLookup c >>= \case
    Just x -> return x
    Nothing -> confLookup "git.enable" >>= \case
               Just (Conf.Bool True) -> queryGit (Text.unpack gitQ) >>= \case
                   Just x -> do setOption c (Conf.String x)
                                return x
                   Nothing -> promptAndSet
               _ -> promptAndSet
  where
    promptAndSet = do
      x <- prompt c p i
      setOption c (Conf.String x)
      return x

firstDirectoryThatExists :: MonadIO m => [FilePath] -> m (Maybe FilePath)
firstDirectoryThatExists ds = liftIO $ go ds
  where
    go [] = return Nothing
    go (d:ds') = do
      exists <- doesDirectoryExist d
      if exists
        then return $ Just d
        else go ds'


-- | Get license from configuration or prompt
getLicense :: MakePackage Text.Text
getLicense = do
    licensesCustomDir <- do
      dataDir <- liftIO $ getXdgDirectory XdgData "make-package"
      return $ dataDir </> "licenses"
    licensesDataDir <- liftIO $ dataFile "licenses"
    licensesDir <-
      firstDirectoryThatExists [licensesCustomDir, licensesDataDir] >>= \case
        Nothing -> do
          err "License directory does not exist"
          liftIO $ exitFailure
        Just ld -> return ld
    licenses <- map takeBaseName . filter (".license" `isSuffixOf` ) <$>
                    liftIO (getDirectoryContents licensesDir)
    defaultLicense <- confLookup "defaults.license" >>= \case
        Nothing -> return Nothing
        Just l -> if l `elem` licenses
                  then return . Just $ Text.pack l
                  else do liftIO . putStrLn $
                              "Warning, configured default license \""
                              ++ l ++ "\" is unknown"
                          return Nothing
    case defaultLicense of
        Just l -> return l
        Nothing -> Text.pack <$> selectFrom "license" licenses True

-- | Query git configuration
queryGit :: String -> MakePackage (Maybe Text.Text)
queryGit q = liftIO (readProcessWithExitCode "git" ["config","--get", q] "")
             >>= \case
    (ExitSuccess, res, _stderr) -> return . Just $ oneLine res
    _ -> return Nothing
  where
    oneLine = Text.concat . take 1 . Text.lines . Text.pack

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
    appDataXdg <- getXdgDirectory XdgConfig "make-package"
    home <- getHomeDirectory
    Conf.load [ Conf.Optional $ appData </> "make-package.conf"
              , Conf.Optional $ appDataXdg </> "make-package.conf"
              , Conf.Optional $ home </> ".make-package.conf"
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
selectFrom :: MonadIO m =>
              String
           -> [String]
           -> Bool -- Strict matching
           -> m String
selectFrom p xs isStrict = liftIO $
  runInputT (setComplete cf defaultSettings) $ do
    HL.outputStrLn "Select one of: "
    HL.outputStrLn options

    go
  where
    pairs = zip (map show [1:: Int ..]) xs
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
           | not isStrict -> return ln
           | "?" <- ln -> HL.outputStrLn options >> go
           | otherwise -> do
               HL.outputStrLn $ ln ++ " is not valid, type ? to show options"
               go
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
