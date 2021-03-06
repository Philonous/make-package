{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Github where

import           Control.Applicative    ((<$>))
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Foldable          as Foldable
import qualified Data.Text              as T
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as T
import qualified GitHub.Auth            as GitHub
import qualified GitHub.Data.Name       as GitHub
import qualified GitHub.Data.Repos      as GitHub
import qualified GitHub.Data.URL        as GitHub
import qualified GitHub.Endpoints.Repos as GitHub
import           System.Exit            (exitSuccess)
import           System.Timeout

import           IO

-- | Check whether a repository with the given name exists on the users github
-- accoutn and suggest cloning it
maybeClone :: T.Text -> T.Text -> MakePackage ()
maybeClone repo dir = unlessConf "github.enable" (== False) $
                      unlessConf "github.never-clone" (== True) $ do
    uname <- confOrPrompt "github.username" "Github Username" ""
    liftIO $ putStrLn "Checking github for preexisting repos"
    timeoutDelay <- toSeconds <$> confLookup "github.timeout"
    mbRepoUrl <- liftIO . timeout timeoutDelay $ getRepo uname
    case mbRepoUrl of
        Just (Right (Just rUrl)) ->
            do cloneP <- promptYesNo "clone"
                                     "A repository with this name already exists\
                                     \in your github account. Clone it instead?"
               when cloneP $ do run "git" [ "clone"
                                          , Text.unpack $ GitHub.getUrl rUrl
                                          , T.unpack dir]
                                liftIO exitSuccess
        _ -> return ()
  where
    toSeconds Nothing = 5 * 10^(6::Int)
    toSeconds (Just t) = t * 10^(6::Int)
    getRepo uname = fmap GitHub.repoSshUrl
                    <$> GitHub.repository (GitHub.N uname) (GitHub.N repo)

-- | Create a github repository in the users account and set it as origin if
-- permitted by configuration
handleGithub :: T.Text -> T.Text -> MakePackage ()
handleGithub repo description = unlessConf "github.enable" (== False) $
                                unlessConf "github.never-create" (== True) $
                                unlessEmptyPrompt "repository" $ \rName ->  do
    auth <- confLookup "github.auth.oauth" >>= \case
        Just oauth -> return $ GitHub.OAuth oauth
        Nothing -> do uname <- confOrPrompt "github.username" "Github Username" ""
                      pwd <- confOrPrompt "github.auth.password"
                                          "Github Password"
                                          ""
                      return $ GitHub.BasicAuth (T.encodeUtf8 uname)
                                                (T.encodeUtf8 pwd)
    let nRepo = GitHub.NewRepo
                { GitHub.newRepoName         = GitHub.N rName
                , GitHub.newRepoDescription  = Just description
                , GitHub.newRepoHomepage     = Nothing
                , GitHub.newRepoPrivate      = Nothing
                , GitHub.newRepoHasIssues    = Nothing
                , GitHub.newRepoHasWiki      = Nothing
                , GitHub.newRepoAutoInit     = Nothing }
    liftIO $ putStrLn "Creating github repository"
    mbRepo <- liftIO $ GitHub.createRepo' auth nRepo
    case mbRepo of
        Left e -> do liftIO $ putStrLn $ "An Error occured while trying to create the repository:" ++ show e
        Right r -> do liftIO $ putStrLn "Done."
                      liftIO $ putStrLn "Configuring git to track the new repository"
                      Foldable.forM_ (GitHub.repoSshUrl r) $ \url ->
                          run "git" ["remote", "add", "origin", Text.unpack $ GitHub.getUrl url]
                      run "git" ["config", "branch.master.remote", "origin"]
                      run "git" [ "config"
                                , "branch.master.merge"
                                , "refs/heads/master"]
                      liftIO $ putStrLn "Done."
                      whenConf "git.do-commit" (== True) $ run "git" [ "push"
                                                                     , "origin"
                                                                     , "master"]
  where
    unlessEmptyPrompt c f =
        do line <- prompt c "Repo Name (blank to skip)> " repo
           if T.null line
           then return ()
           else f line
