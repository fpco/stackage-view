-- | Main FPView display.

module Handler.Home where

import Import
import Yesod.GHCJS

getHomeR :: Handler Html
getHomeR =
  defaultLayout
    (do setTitle "FPView"
        $(widgetFile "home")
        toWidget [hamlet|<script src=@{GhcjsHomeR}>|])

getGhcjsHomeR :: Handler TypedContent
getGhcjsHomeR =
  $(ghcjsFileDev
      development
      ["-iclient"
      ,"-ishared"
      ,"-XCPP"
      ,"-XTemplateHaskell"
      ,"-package"
      ,"ghcjs-react"]
      "app/Client.hs")
