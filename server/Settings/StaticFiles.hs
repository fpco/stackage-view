module Settings.StaticFiles where

import           Data.Default (def)
import           Language.Haskell.TH (Q, Exp, Name)
import           Prelude (IO, return)
import           Settings (staticDir)
import           Settings.Development
-- import           Yesod.GHCJS
import           Yesod.Static
import qualified Yesod.Static as Static

-- import           Settings.GhcjsFiles

-- | use this to create your static file serving site
staticSite :: IO Static.Static
#if DEVELOPMENT
staticSite = Static.staticDevel staticDir
#else
staticSite = return $(embed "static")
#endif

-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
$(staticFiles Settings.staticDir)

combineSettings :: CombineSettings
combineSettings = def

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets' development combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts' development combineSettings
