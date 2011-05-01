{-# LANGUAGE TypeOperators #-}

module Example where

import Iso
import JsonGrammar

import Prelude hiding (id, (.), head)
import Control.Category


data Attachment = Attachment
  { atData        :: Maybe String
  , atDescription :: String
  , atType        :: AttachmentType
  , atName        :: String
  , atThumbnail   :: Maybe String
  } deriving Show

data AttachmentType = AttVideo | AttImage | AttHtml
  deriving Show


-- Constructor isomorphisms
-- These will later be derived using TH

attVideo :: Iso t (AttachmentType :- t)
attVideo = Iso f g
  where
    f t = Just (AttVideo :- t)
    g (AttVideo :- t) = Just t
    g _               = Nothing

attImage :: Iso t (AttachmentType :- t)
attImage = Iso f g
  where
    f t = Just (AttImage :- t)
    g (AttImage :- t) = Just t
    g _               = Nothing

attHtml :: Iso t (AttachmentType :- t)
attHtml = Iso f g
  where
    f t = Just (AttHtml :- t)
    g (AttHtml :- t) = Just t
    g _               = Nothing

attachment :: Iso (Maybe String :- String :- AttachmentType :- String :-
  Maybe String :- t) (Attachment :- t)
attachment = Iso f g
  where
    f (x0 :- x1 :- x2 :- x3 :- x4 :- t) =
      return (Attachment x0 x1 x2 x3 x4 :- t)
    g (Attachment x0 x1 x2 x3 x4 :- t) =
      Just (x0 :- x1 :- x2 :- x3 :- x4 :- t)


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
