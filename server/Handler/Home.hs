-- | Main Stackage View display.

module Handler.Home where

import Import
import Yesod.GHCJS

getHomeR :: Handler Html
getHomeR =
  defaultLayout
    (do setTitle "Stackage View"
        $(widgetFile "home")
        toWidget [hamlet|<script src=@{GhcjsHomeR}>|])

getGhcjsHomeR :: Handler TypedContent
getGhcjsHomeR =
  $(ghcjsFileDev
      development
      (["-iclient"
      ,"-ishared"
      ,"-XCPP"
      ,"-XTemplateHaskell"
      ,"-hide-all-packages"] ++ concatMap (\pkg -> ["-package", pkg])
        ["aeson"
        ,"attoparsec"
        ,"base"
        ,"containers"
        ,"data-default"
        ,"ghcjs-base"
        ,"ghcjs-dom"
        ,"ghcjs-jquery"
        ,"ghcjs-react"
        ,"lens"
        ,"mtl"
        ,"stm"
        ,"text"
        ])
      ["client", "shared"]
      "app/Client.hs")
