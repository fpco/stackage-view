{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Handy client-server communication.

module GHCJS.Yesod where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode (encodeToTextBuilder)
import           Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.Default
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Typeable
import           Prelude
import           SharedTypes

#ifdef __GHCJS__
import           JavaScript.JQuery
#endif

-- | An exception when making an AJAX call.
data CallException
  = BadStatusCode !Int
  | DecodingError !String !Text
  | NoData
  deriving (Show,Typeable)
instance Exception CallException

-- | Call a server-side command.
call :: (FromJSON a)
     => (Returns a -> Command) -> IO a
call mkCmd =
  do AjaxResult status mdata <-
       ajax ("/command" :: Text) params settings
     case status of
       200 ->
         do text <-
              maybe (throwIO NoData) return mdata
            case parseOnly value (encodeUtf8 text) >>=
                 parseEither parseJSON of
              Left s ->
                throwIO (DecodingError s text)
              Right v -> return v
       _ -> throwIO (BadStatusCode status)
  where cmd = mkCmd Returns
        cmdText = TL.toStrict $ toLazyText $ encodeToTextBuilder $ toJSON cmd
        params = [("json" :: Text,cmdText)]
        settings = def {asMethod = POST}

-- Just so that I can type-check this module with GHCi.
#ifndef __GHCJS__
ajax = error "GHCJS.Yesod.ajax"
data POST = POST
data AJAXQ = AJAXQ {asMethod :: POST}
instance Default AJAXQ where def = error "GHCJS.AJAXQ"
data AjaxResult = AjaxResult Int (Maybe Text)
instance Default AjaxResult where def = error "GHCJS.AjaxResult"
#endif
