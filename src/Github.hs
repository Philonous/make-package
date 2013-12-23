{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Github where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Github.Repos
import           System.Console.Haskeline as HL
import           System.Exit (exitSuccess)
import           System.Timeout

import           IO

-- | Check whether a repository with the given name exists on the users github
-- accoutn and suggest cloning it
maybeClone :: T.Text -> MakePackage ()
maybeClone repo = unlessConf "github.enable" (== False) $
                  unlessConf "github.never-clone" (== True) $ do
    uname <- confOrPrompt "github.username" "Github Username"
    liftIO $ putStrLn "Checking github for preexisting repos"
    timeoutDelay <- toSeconds <$> confLookup "github.timeout"
    mbRepoUrl <- liftIO . timeout timeoutDelay $ getRepo uname
    case mbRepoUrl of
        Nothing -> return ()
        Just (Left _) -> return ()
        Just (Right rUrl) ->
            do cloneP <- promptYesNo "A repository with this name already exists\
                                     \in your github account. Clone it instead?"
               when cloneP $ do run "git" ["clone", rUrl]
                                liftIO exitSuccess
  where
    toSeconds Nothing = 5 * 10^(6::Int)
    toSeconds (Just t) = t * 10^(6::Int)
    getRepo uname = fmap repoSshUrl <$> userRepo (T.unpack uname) (T.unpack repo)

-- | Create a github repository in the users account and set it as origin if
-- permitted by configuration
handleGithub :: T.Text -> T.Text -> MakePackage ()
handleGithub repo description = unlessConf "github.enable" (== False) $
                                unlessConf "github.never-create" (== True) $
                                unlessEmptyPrompt $ \rName ->  do
    auth <- confLookup "github.auth.oauth" >>= \case
        Just oauth -> return $ GithubOAuth oauth
        Nothing -> do uname <- confOrPrompt "github.username" "Github Username"
                      pwd <- confOrPrompt "github.password" "Github Password"
                      return $ GithubBasicAuth (T.encodeUtf8 uname)
                                               (T.encodeUtf8 pwd)
    let nRepo = NewRepo { newRepoName         = T.unpack rName
                        , newRepoDescription  = Just (T.unpack description)
                        , newRepoHomepage     = Nothing
                        , newRepoPrivate      = Nothing
                        , newRepoHasIssues    = Nothing
                        , newRepoHasWiki      = Nothing
                        , newRepoAutoInit     = Nothing }
    liftIO $ putStrLn "Creating github repository"
    mbRepo <- liftIO $ createRepo auth nRepo
    case mbRepo of
        Left e -> do liftIO $ putStrLn $ "An Error occured while trying to create the repository:" ++ show e
        Right r -> do liftIO $ putStrLn "Done."
                      liftIO $ putStrLn "Configuring git to track the new repository"
                      run "git" ["remote", "add", "origin", repoSshUrl r]
                      run "git" ["config", "branch.master.remote", "origin"]
                      run "git" [ "config"
                                , "branch.master.merge"
                                , "refs/heads/master"]
                      liftIO $ putStrLn "Done."
                      whenConf "git.do-commit" (== True) $ run "git" [ "push"
                                                                     , "origin"
                                                                     , "master"]
  where
    unlessEmptyPrompt f =
        do ln <- liftIO $ runInputT defaultSettings $ HL.getInputLineWithInitial
                                                      "Repo Name (blank to skip)> "
                                                      ("", T.unpack repo )
           case ln of
               Nothing -> return ()
               Just line | null line -> return ()
                         | otherwise -> f (T.pack line)
