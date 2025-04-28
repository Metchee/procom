module Document.Types where

import Data.Maybe (isJust)

-- Structure de document
data Document = Document
  { docHeader :: Header
  , docBody   :: [Content]
  } deriving (Show, Eq)

-- En-tête du document
data Header = Header
  { headerTitle  :: String
  , headerAuthor :: Maybe String
  , headerDate   :: Maybe String
  } deriving (Show, Eq)

-- Contenu du document
data Content
  = Text String
  | Italic Content
  | Bold Content
  | Code String
  | Link String String  -- Text et URL
  | Image String String -- Alt text et URL
  | Paragraph [Content]
  | Section String [Content]  -- Titre et contenu
  | CodeBlock String          -- Bloc de code
  | List [Item]
  deriving (Show, Eq)

-- Élément de liste
data Item = Item [Content] deriving (Show, Eq)

-- Fonctions utilitaires pour créer les documents
makeDocument :: String -> Maybe String -> Maybe String -> [Content] -> Document
makeDocument title author date contents = Document (Header title author date) contents

makeSimpleDocument :: String -> [Content] -> Document
makeSimpleDocument title contents = makeDocument title Nothing Nothing contents