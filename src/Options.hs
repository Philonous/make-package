{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}

module Options where

import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Text as T
import           System.Console.GetOpt
import           System.Environment
import           System.Exit (exitSuccess, exitFailure)

import           IO

setOpt :: T.Text -> String -> ArgDescr (MakePackage ())
setOpt o = ReqArg (setOption o . T.pack)

options :: [OptDescr (MakePackage ())]
options = [ Option "h" ["help"] (NoArg $ help [])
              "display this help text"
          , Option "P" ["package"] (setOpt "package" "string")
              "package name"
          , Option "d" ["dir"] (setOpt "directory" "string")
            "directory to initialize in (default = package name)"
          , Option "A" ["author"] (setOpt "defaults.author" "string")
            "package author"
          , Option "E" ["email"] (setOpt "defaults.email" "string")
            "package author email address"
          , Option "D" ["description"] (setOpt "description" "string")
            "package description"
          , Option "L" ["license"] (setOpt "defaults.license" "string")
            "license name"
          , Option "M" ["modules"] (setOpt "modules" "string")
            "exposed modules"
          , Option "H" ["categories"] (setOpt "categories" "string")
            "hackage categories"
          , Option "G" ["git"] (NoArg $ setOption "git.enable" "true")
            "enable git"
          , Option "g" ["no-git"] (NoArg $ setOption "git.enable" "false")
            "disable git"
          , Option "C" ["commit"] (NoArg $ setOption "git.do-commit" "true")
            "run git commit"
          , Option "c" ["no-commit"] (NoArg $ setOption "git.do-commit" "false")
            "don't run git commit"
          , Option "M" ["commit-message"] (setOpt "git.initial-commit-message"
                                                  "string")
            "commit message to use"
          , Option "t" ["template"] (setOpt "template.name" "string") "template"
          , Option "G" ["github"] (NoArg $ setOption "github.enable" "true")
            "enable github integration"
          , Option "g" ["no-github"] (NoArg $ setOption "github.enable" "false")
            "disable github integration"
          , Option "U" ["username"] (setOpt "github.username" "string")
            "github username"
          , Option "R" ["repository"] (setOpt "github.repository" "string")
            "github repository to create"
          , Option "T" ["timeout"] (setOpt "github.timeout" "int")
            "timeout for github interactions in seconds"
          , Option "K" ["clone"] (NoArg $ setOption "clone" "true")
            "clone existing repository "
          , Option "k" ["no-clone"] (NoArg $ setOption "clone" "false")
            "don't clone existing repository "
          , Option "O" ["oauth"] (setOpt "github.auth.oauth" "string")
            "github oauth token"
          , Option "P" ["password"] (setOpt "github.auth.password" "string")
            "github password"
          ]

help :: MonadIO m => [String] -> m a
help errors = liftIO $
  do forM_  errors putStrLn
     putStrLn $ usageInfo "make-package [package-name] [options]" options
     if null errors
     then exitSuccess
     else exitFailure

handleArgs :: MakePackage ()
handleArgs =
  do args <- liftIO $ getArgs
     let (opts, _, errs) = getOpt (ReturnInOrder (setOption "package" . T.pack))
                                  options
                                  args
     unless (null errs) $ help errs
     sequence_ opts
