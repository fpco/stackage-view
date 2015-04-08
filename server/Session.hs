-- | Session functions.

module Session where

import           Control.Arrow hiding (app)
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import           Distribution.Text (display)
import           FPView.Cabal
import           FPView.Git
import           FPView.Types
import           Filesystem as FP
import           Filesystem.Loc as FL
import           Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import           IdeSession hiding (errorSpan,errorMsg)
import           Import hiding (FilePath)
import           Language.Haskell.Extension
import           Prelude hiding (FilePath,writeFile,pi)
import           SharedTypes

-- | Continue the session with the given unambiguous targets.
continueWith :: [TargetIdent] -> App -> IO ()
continueWith targets app =
  do unloadTargets (appSession app) (appLoadedTargets app) targets
     loadProject
       (appSession app)
       (M.fromList
          (map (\target ->
                  (targetIdent target,any (== targetIdent target) targets))
               (appTargets app)))
       (mapMaybe (\ident ->
                    find ((== ident) . targetIdent)
                         (appTargets app))
                 targets)
       (appDataFiles app)
       (appLoading app)
       (appLoadedTargets app)

-- | Unload targets.
unloadTargets :: IdeSession -> TVar [Target] -> [TargetIdent] -> IO ()
unloadTargets sess loadedTargets except =
  do targets <- atomically (readTVar loadedTargets)
     let allTargetFiles =
           mconcat (map targetFiles
                        (filter (not .
                                 flip elem except .
                                 targetIdent)
                                targets))
     updateSession
       sess
       (mconcat (map (updateSourceFileDelete . FL.encodeString)
                     (S.toList allTargetFiles)))
       (const (return ()))

-- | Start the session with the cabal file, will not proceed if the
-- targets are unambiguous, in which case it will be continued later
-- after prompting the user.
startSession :: TVar [Target]
             -> TVar LoadingStatus
             -> IO (IdeSession,[Target],Set FilePath)
startSession loadedTargets loading =
  do dir <- FL.getWorkingDir
     cabalfp <- getCabalFp dir
     targets <- getTargets mempty cabalfp
     datafiles <- getGitFiles >>= fmap S.fromList . filterM isFile . S.toList
     session <-
       initSession (sessionParams targets)
                   defaultSessionConfig
     if length (filter (needsMain . targetIdent) targets) >
        1
        then atomically (writeTVar loading (AmbiguousTargets (map targetIdent targets)))
        else loadProject
               session
               (M.fromList
                  (map (targetIdent &&&
                        const True)
                       targets))
               targets
               datafiles
               loading
               loadedTargets
     return (session,targets,datafiles)
  where sessionParams targets =
          defaultSessionInitParams {sessionInitGhcOptions =
                                      ["-hide-all-packages"] <>
                                      concatMap includePackage
                                                (concatMap targetDependencies targets)}
          where includePackage pkgName =
                  ["-package",display pkgName]

-- | Load the project into the ide-backend.
loadProject :: IdeSession
            -> Map TargetIdent Bool
            -> [Target]
            -> Set FilePath
            -> TVar LoadingStatus
            -> TVar [Target]
            -> IO ()
loadProject session targetChoices targets datafiles loading loadedTargets =
  void (forkIO (do atomically (writeTVar loadedTargets targets)
                   setOpts session targets
                   loadFiles session targetChoices targets datafiles loading))

-- | Set GHC options.
setOpts  :: IdeSession -> [Target] ->  IO ()
setOpts sess targets =
  updateSession sess
                (updateGhcOpts (map showExt (concatMap targetExtensions targets)))
                (const (return ()))
  where showExt :: Extension -> String
        showExt g =
          case g of
            EnableExtension e -> "-X" <> show e
            DisableExtension e -> "-XNo" <> show e
            UnknownExtension e -> "-X" <> show e

-- | Load the package files and update the app state of the progress.
loadFiles :: IdeSession
          -> Map TargetIdent Bool
          -> [Target]
          -> Set FilePath
          -> TVar LoadingStatus
          -> IO ()
loadFiles sess targetChoices targets files loading =
  do updates <- forM loadedFiles
                     (\fp ->
                        do content <- L.readFile (FL.encodeString fp)
                           return (updateSourceFile (FL.encodeString fp)
                                                    content))
     updates' <- forM (S.toList files)
                      (\fp ->
                         do content <- L.readFile (FP.encodeString fp)
                            return (updateDataFile (FP.encodeString fp)
                                                   content))
     atomically (writeTVar loading NotLoading)
     updateSession
       sess
       (mconcat updates <> mconcat updates' <>
        updateCodeGeneration True)
       (\progress ->
          atomically
            (writeTVar loading
                       (Loading (progressStep progress)
                                (progressNumSteps progress)
                                (fromMaybe (fromMaybe "Unknown step" (progressOrigMsg progress))
                                           (progressParsedMsg progress)))))
     errs <- fmap (filter isError)
                  (getSourceErrors sess)
     if null errs
        then atomically
               (writeTVar loading
                          (LoadOK (M.toList targetChoices)
                                  (map (T.pack . FL.encodeString)
                                       (sort loadedFiles))))
        else atomically (writeTVar loading (LoadFailed (map toError errs)))
  where loadedFiles =
          S.toList (mconcat (map targetFiles targets))
        isError (SourceError{errorKind = k}) =
          case k of
            KindError -> True
            KindServerDied -> True
            KindWarning -> False

-- | Convert a source error to either an unhelpful text span or a
-- proper span.
toError :: SourceError -> Either Text Error
toError (SourceError _ espan msg) =
  case espan of
    ProperSpan (SourceSpan path sl sc el ec) ->
      Right (Error {errorFile = T.pack path
                   ,errorSpan = Span sl sc el ec
                   ,errorMsg = msg})
    TextSpan e -> Left e
