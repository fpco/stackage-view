{-# LANGUAGE OverloadedStrings #-}

-- | Loading screen.

module View.LoadingProject where

import           Control.Monad
import           Data.Monoid
import qualified Data.Text as T
import           Prelude hiding (pi)
import           React
import           React.Lucid
import           SharedTypes
import           View.Template

-- | Loading screen.
loading :: Monad m
        => LoadingStatus -> ReactT state m ()
loading prog =
  div_ (do class_ "loading"
           centered_ page)
  where screenInfo inner =
          p_ (do class_ "screen-info"
                 inner)
        page =
          do img_ (src_ "/static/img/loading.png")
             case prog of
               NotLoading ->
                 screenInfo "Initializing\8230"
               Loading n total msg ->
                 do progressBar n total
                    screenInfo
                      (text (T.pack (show n) <>
                             "/" <>
                             T.pack (show total) <>
                             ": " <>
                             msg))
               _ -> screenInfo "Done!"

-- | Progress bar display for loading screen.
progressBar :: Monad m
            => Int -> Int -> ReactT state m ()
progressBar n total =
  div_ (do class_ "prog-bar"
           div_ (do class_ "prog"
                    style "width"
                          (T.pack (show ratio) <>
                           "px")))
  where ratio :: Int
        ratio =
          round (width *
                 (n' / total'))
        width = 250 :: Double
        n' = fromIntegral n
        total' = fromIntegral total
