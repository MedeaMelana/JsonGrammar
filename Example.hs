{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

module Example where

import Data.Iso.Core
import Data.Iso.TH
import Language.JsonGrammar

import Prelude hiding (id, (.), head, either)
import Control.Category


data Attachment = Attachment
  { atData        :: Maybe String
  , atDescription :: String
  , atType        :: AttachmentType
  , atName        :: String
  , atThumbnail   :: Maybe String
  } deriving Show

attachment :: Iso (Maybe String :- String :- AttachmentType :-
  String :- Maybe String :- t) (Attachment :- t)
attachment = $(deriveIsos ''Attachment)


data AttachmentType = AttVideo | AttImage | AttHtml
  deriving Show

attVideo :: Iso a (AttachmentType :- a)
attImage :: Iso a (AttachmentType :- a)
attHtml  :: Iso a (AttachmentType :- a)
(attVideo, attImage, attHtml) = $(deriveIsos ''AttachmentType)


-- Json grammars

instance Json Attachment where
  grammar = attachment . object
    ( prop "data"
    . prop "description"
    . prop "type"
    . prop "name"
    . prop "thumbnail"
    )

instance Json AttachmentType where
  grammar = attVideo . litJson "video"
              <> attImage . litJson "image"
              <> attHtml  . litJson "html"
