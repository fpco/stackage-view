{-# LANGUAGE OverloadedStrings #-}

-- | Loading failed.

module View.Failed where

import           Control.Monad
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (pi)
import           React
import           React.Lucid
import           SharedTypes
import           View.Template

-- | Failed to load the project.
failed :: Monad m
       => [Either Text Error] -> ReactT state m ()
failed errs =
  div_ (do class_ "failed"
           centered_ (do h1_ "A problem occurred"
                         info))
  where info =
          div_ (do class_ "screen-info"
                   forM_ errs renderError)
        renderError e =
          div_ (case e of
                  Left u -> p_ (text u)
                  Right (Error fp sp msg) ->
                    do p_ (do strong_ (text fp)
                              text ":"
                              renderSpan sp)
                       pre_ (text msg))

-- | Render a span.
renderSpan :: Monad m => Span -> ReactT state m ()
renderSpan (Span sl sc el ec) =
  text (T.pack (show sl <> ":" <> show sc <> "-" <> show el <> ":" <> show ec))
