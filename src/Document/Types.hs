{-
-- EPITECH PROJECT, 2025
-- procom
-- File description:
-- Types
-}

module Document.Types where

import Data.Maybe (isJust)

data Document = Document {
  docHeader :: Header,
  docContent :: [Content]
} deriving (Show, Eq)

data Header = Header {
  headerTitle  :: String,
  headerAuthor :: Maybe String,
  headerDate   :: Maybe String
} deriving (Show, Eq)

data Content
  = Text String
  | Italic Content
  | Bold Content
  | Code String
  | Link String String
  | Image String String
  | Paragraph [Content]
  | Section (Maybe String) [Content]
  | CodeBlock [Content]
  | List [Item]
  deriving (Show, Eq)

data Item = Item [Content] deriving (Show, Eq)

makeDocument :: String -> Maybe String -> Maybe String -> [Content] -> Document
makeDocument title author date contents =
  Document (Header title author date) contents

makeSimpleDocument :: String -> [Content] -> Document
makeSimpleDocument title contents = makeDocument title Nothing Nothing contents