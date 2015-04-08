-- | Main entry point.

import Application (runApplication)
import Control.Monad
import Data.Maybe
import Data.Monoid
import Network.Wai.Handler.Warp
import Prelude
import System.Environment
import Text.Read

import Options.Applicative

data MyApp = MyApp { appPort :: Port }

runWithOptions :: MyApp -> IO ()
runWithOptions opts =
  do putStrLn ("Point your browser to http://localhost:" ++ show (appPort opts) ++ "/")
     runApplication (appPort opts)

main :: IO ()
main = do mport <- fmap (bind readMaybe . lookup "PORT") getEnvironment
          bind runWithOptions
               (execParser (info (parser mport) mempty))
  where parser mport =
          MyApp <$>
          fmap (fromMaybe (fromMaybe 3000 mport) . bind readMaybe)
               (optional (strOption (long "port" <>
                                     metavar "PORT" <>
                                     help "Port to listen on")))
        bind :: Monad m => (a -> m b) -> m a -> m b
        bind = (=<<)
