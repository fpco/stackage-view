module Foundation where

import Control.Concurrent.STM.TVar
import Data.Set (Set)
import FPView.Types
import IdeSession
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import Prelude hiding (FilePath)
import Settings (widgetFile)
import Settings.Development (development)
import Settings.StaticFiles
import SharedTypes
import Text.Hamlet (hamletFile)
import Yesod
import Yesod.Core.Types (Logger)
import Filesystem.Path (FilePath)
import Yesod.Static

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { getStatic        :: Static -- ^ Settings for static file serving.
    , httpManager      :: Manager
    , appLogger        :: Logger
    , appSession       :: IdeSession
    , appLoading       :: TVar LoadingStatus
    , appLastLoading   :: TVar LoadingStatus
    , appTargets       :: [Target]
    , appLoadedTargets :: TVar [Target]
    , appDataFiles     :: Set FilePath
    }

instance HasHttpManager App where
    getHttpManager = httpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootRelative

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = return Nothing

    defaultLayout widget = do
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
