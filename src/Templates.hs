{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Templates where

import           Control.Arrow            (second)
import           Control.Monad
import qualified Control.Monad.Catch      as Ex
import           Control.Monad.Trans
import qualified Data.Aeson               as Aeson
import qualified Data.Foldable            as Foldable
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe)
import           Data.Monoid
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Data.Text.Lazy           (toStrict)
import qualified System.Directory         as Dir
import           System.Exit
import           System.FilePath
import qualified System.IO.Temp           as Tmp
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
  -- ^ File and directory names to be ignored
  , ignoreFiles :: Set FilePath
  -- ^ Renames based on filenames
  , baseDir :: FilePath
  }

renderTemplates :: TemplateConfig -> FilePath -> FilePath -> MakePackage ()
renderTemplates conf from to = do
  dirContents <- liftIO $ Dir.listDirectory from
  forM_ (filter (not . ignore) dirContents) $ \f -> do
    let file = fromFile f
    mbStatus <- liftIO . Ex.try $ getFileStatus file
    case mbStatus of
      Right status
        | isRegularFile status -> handleRegularFile f
          -- Recurse into directories
        | isDirectory status -> handleDirectory f
        | otherwise -> skip file
      Left (_ :: IOError) -> err $ "Can't read file " <> Text.pack file

  where
    fromFile f = from </> f
    toFile f = to </> fromMaybe f (Map.lookup f (renames conf))
    ignore x = x `Set.member` (ignoreFiles conf)
    skip f = warn $ "Can't read " <> Text.pack f <> ". Skipping."
    handleRegularFile f =
      case Map.lookup (takeExtension f) (templateHandlers conf) of
        Just tmpl -> runTmpl tmpl f
        Nothing -> do
          copyVerbatim f
    runTmpl tmpl f =
      liftIO . Text.writeFile (toFile (dropExtension f)) =<< tmpl (fromFile f)
    copyVerbatim f = liftIO $ Dir.copyFile (fromFile f) (toFile f)
    handleDirectory f = do
      let target = to </> f
      liftIO $ Dir.createDirectoryIfMissing False target
      renderTemplates conf (from </> f) target


templates :: Text -> [(Text, Text)] -> FilePath -> TemplateConfig
templates packageName substitutions baseDir =
      -- Map from filename extensions to template handlers
      -- e.g. "Main.hs.mustache" will be handled by the mustache template engine
  let templateHandlers =
        Map.fromList
          [ (".mustache", mustache $ toObject substitutions)
          , (".template", simpleTemplate substitutions)
          ]
      ignoreFiles = Set.fromList [".git"]
      -- Files to be renamed
      renames =
        Map.fromList [("package.cabal", Text.unpack packageName <> ".cabal")]
  in TemplateConfig {..}
  where
    toObject = Aeson.object . map (second Aeson.toJSON)

-- | Polymorphic version of directory's withCurrentDirectory
withCurrentDirectory :: (Ex.MonadMask m, MonadIO m) => FilePath -> m b -> m b
withCurrentDirectory dir f = do
  pwd <- liftIO $ Dir.getCurrentDirectory
  Ex.bracket_ (liftIO $ Dir.setCurrentDirectory dir)
              (liftIO $ Dir.setCurrentDirectory pwd)
              f

-- | Load template from a directory
runDirTemplate :: TemplateConfig
               -> Text -- ^ Source directory
               -> MakePackage ()
runDirTemplate conf dir = do
  absDir <- liftIO $ Dir.makeAbsolute (Text.unpack dir)
  isDir <- liftIO $ Dir.doesDirectoryExist absDir
  unless isDir $ do
    err $ dir <> " does not exist or is not a directory."
    liftIO $ exitFailure
  info $ "Copying template from " <> (Text.pack absDir)
  renderTemplates conf (Text.unpack dir) (baseDir conf)

-- | Load template from git repository
--
-- The repository will be cloned into a temporary directory and then copied via
-- 'runDirTemplate'
runGitTemplate ::
     TemplateConfig
  -> Text -- ^ Reporitory
  -> MakePackage ()
runGitTemplate conf repository =
  Tmp.withSystemTempDirectory "make-package" $ \dir -> do
    run "git" ["clone", Text.unpack repository, dir]
    liftIO $ Dir.removeDirectoryRecursive (dir </> ".git")
    runDirTemplate conf (Text.pack dir)

-- | Load template from github.
runGithubTemplate :: TemplateConfig -> Text -> MakePackage ()
runGithubTemplate conf path =
  runGitTemplate conf ("https://github.com/" <> path <> ".git")

getTemplateDir :: MakePackage FilePath
getTemplateDir = do
  defaultTemplateDir <- liftIO $ Dir.getXdgDirectory Dir.XdgData "make-package"
  (</> "templates") <$> optionDefault "template.directory" defaultTemplateDir

getTemplateNames :: MakePackage [FilePath]
getTemplateNames = do
  templateDir <- getTemplateDir
  Ex.try (liftIO $ Dir.listDirectory templateDir) >>= \case
    Left (_ :: IOError) -> do
      err $
        "Could not read template directory (" <> Text.pack templateDir <> ")"
      liftIO $ exitFailure
    Right templateNames -> return templateNames

-- | Load a named template from the template directory
runNamedTemplate ::
     TemplateConfig
  -> Text -- ^ Chosen template
  -> MakePackage ()
runNamedTemplate conf template = do
  templateDir <- getTemplateDir
  renderTemplates conf (templateDir </> (Text.unpack template)) (baseDir conf)

selectTemplate :: MakePackage Text
selectTemplate = do
  templateNames <- getTemplateNames
  Text.pack <$> selectFrom "Template" templateNames False


-- | Parse a template URI to see where we fetch the template from
runTemplate :: TemplateConfig
            -> Maybe Text -- ^ Template URI
            -> MakePackage ()
runTemplate conf mbTempl = do
  template <- case mbTempl of
                Just tmpl -> return tmpl
                Nothing -> selectTemplate
  -- Template URIs starting with "git:", "github:" or "file:" should be treated
  -- specially. We assume it's a named template if it doesn't start with any of
  -- those prefixes
  if | (selector, rest) <- Text.breakOn delim template
     , Just uri <- Text.stripPrefix delim rest
     , not (Text.null uri)
       -> case selector of
            "git" -> runGitTemplate conf uri
            "github" -> runGithubTemplate conf uri
            "file" -> runDirTemplate conf uri
            _ -> do
              err $ "Unknown template uri type: " <> selector
              liftIO $ exitFailure
     | otherwise -> runNamedTemplate conf template
  where
    delim = ":"
