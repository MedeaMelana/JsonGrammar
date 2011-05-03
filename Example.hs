{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

module Example where

import Iso
import JsonGrammar
import TH

import Prelude hiding (id, (.), head)
import Control.Category


data Attachment = Attachment
  { atData        :: Maybe String
  , atDescription :: String
  , atType        :: AttachmentType
  , atName        :: String
  , atThumbnail   :: Maybe String
  } deriving Show

attachment = $(deriveIsos ''Attachment)

data AttachmentType = AttVideo | AttImage | AttHtml
  deriving Show

(attVideo, attImage, attHtml) = $(deriveIsos ''AttachmentType)


-- Json grammars

instance Json Attachment where
  grammarStack = attachment . object
    ( prop "data"
    . prop "description"
    . prop "type"
    . prop "name"
    . prop "thumbnail"
    )

instance Json AttachmentType where
  grammarStack = attVideo . lit "video"
              <> attImage . lit "image"
              <> attHtml  . lit "html"
