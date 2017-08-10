{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module Templates where

import           Control.Arrow            (second)
import           Control.Monad
import qualified Control.Monad.Catch      as Ex
import           Control.Monad.Trans
import qualified Data.Aeson               as Aeson
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe)
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Data.Text.Lazy           (toStrict)
import qualified System.Directory         as Dir
import           System.FilePath
import           System.PosixCompat.Files
import qualified Text.Microstache         as Mustache

import           IO

mustache :: Aeson.Value -> FilePath -> MakePackage Text
mustache val from = do
  template <- liftIO $ Mustache.compileMustacheFile from
  let (warnings, text) = Mustache.renderMustacheW template val
  forM_ warnings $ \w ->
    warn $ Text.concat [ "In Mustache template "
                       , Text.pack from, ": ",
                         Text.pack $ Mustache.displayMustacheWarning w
                       ]
  return $ toStrict text

simpleTemplate :: [(Text, Text)] -> FilePath -> MakePackage Text
simpleTemplate substitutions infile  = do
  template <- liftIO $ Text.readFile infile
  return $ substitute template substitutions
  where
    substitute = foldl (\str (this,that) -> Text.replace ("$" <> this) that str)

data TemplateConfig =
  TemplateConfig
  { templateHandlers :: Map FilePath (FilePath -> MakePackage Text)
  -- ^ Filename Extensions to templating engine
  , renames :: Map FilePath FilePath
  -- ^ Renames based on filenames
  , baseDir :: FilePath
  }

renderTemplates :: TemplateConfig -> FilePath -> FilePath -> MakePackage ()
renderTemplates conf from to = do
  dirContents <- liftIO $ Dir.listDirectory from
  forM_ dirContents $ \f -> do
    let file = fromFile f
    mbStatus <- liftIO . Ex.try $ getFileStatus file
    case mbStatus of
      Right status | isRegularFile status -> handleRegularFile f
                   | isDirectory status -> handleDirectory f
                   | otherwise -> skip file
      Left (_ :: IOError) -> err $ "Can't read file " <> Text.pack file
  where
    -- Construct full file path from filename
    fromFile f = baseDir conf </> from </> f
    -- construct full file path from filename
    toFile f = to </> fromMaybe f (Map.lookup f (renames conf))
    skip f = warn $ "Can't read " <> Text.pack f <> ". Skipping."
    handleRegularFile f =
      case Map.lookup (takeExtension f) (templateHandlers conf) of
        Just tmpl -> runTmpl tmpl f
        Nothing -> do
          warn $ Text.pack f <> " : " <> Text.pack (takeExtension f)
          copyVerbatim f
    runTmpl tmpl f = liftIO . Text.writeFile (toFile f) =<< tmpl (fromFile f)
    copyVerbatim f = liftIO $ Dir.copyFile (fromFile f) (toFile f)
    handleDirectory f = do
      let target = to </> f
      liftIO $ Dir.createDirectoryIfMissing False target
      renderTemplates conf (from </> f) target

templates :: Text -> [(Text, Text)] -> TemplateConfig
templates packageName substitutions =
  let templateHandlers = Map.fromList [(".mustache", mustache $ toObject substitutions)
                                      ,(".template", simpleTemplate substitutions)
                                      ]
      renames = Map.fromList [ ("package.cabal", Text.unpack packageName <> ".cabal")]
      baseDir = "."
  in TemplateConfig{..}
  where
    toObject = Aeson.object . map(second Aeson.toJSON)

runTemplates :: TemplateConfig -> MakePackage ()
runTemplates conf = do
  defaultTemplateDir <- liftIO $ Dir.getXdgDirectory Dir.XdgData "make-package"
  templateDir <-
    (</> "templates") <$> optionDefault "template.directory" defaultTemplateDir
  Ex.try (liftIO $ Dir.listDirectory templateDir) >>= \case
    Left (_ :: IOError) ->
      warn $
      "Could not read template directory (" <> Text.pack templateDir <> ")"
    Right templateNames -> do
      template <- selectFrom "Template" templateNames
      renderTemplates conf (templateDir </> template) (baseDir conf)
  return ()
