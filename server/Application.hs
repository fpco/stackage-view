{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    , runApplication
    ) where

import           Control.Concurrent.STM.TVar
import           Data.Default (def)
import           Data.FileEmbed (embedFile)
import           Network.Wai.Handler.Warp


import           Import hiding (FilePath)

import           Network.HTTP.Client.Conduit (newManager)

import           Network.Wai.Logger (clockDateCacher)
import           Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger

import           System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import           Yesod.Core.Types (loggerSet, Logger (Logger))
import           Yesod.Default.Config
import           Yesod.Default.Main

-- Don't forget to add new modules to your cabal file!

import           Handler.Command
import           Handler.Home
import           Prelude hiding (FilePath,writeFile,pi)
import           Session
import           SharedTypes

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: IO Application
makeApplication = do
    foundation <- makeFoundation

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging app

runApplication :: Port -> IO ()
runApplication port = do
    app <- makeApplication
    run port app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: IO App
makeFoundation = do
    manager <- newManager
    s <- staticSite

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    loadingVar <- newTVarIO NotLoading
    lastLoadingVar <- newTVarIO NotLoading
    loadedVar <- newTVarIO []

    (session,targets,files) <- liftIO (startSession loadedVar loadingVar)

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App
            { getStatic = s
            , httpManager = manager
            , appLogger = logger
            , appLoading = loadingVar
            , appLastLoading = lastLoadingVar
            , appSession = session
            , appTargets = targets
            , appDataFiles = files
            , appLoadedTargets = loadedVar
            }

    return foundation

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (const makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
