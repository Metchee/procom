module Parser.Detect where

import Document.Types
import Parser.XML (parseXML)
import Parser.JSON (parseJSON)
import Parser.Markdown (parseMarkdown)
import Data.List (isPrefixOf)
import Data.Char (isSpace)

-- Types de format supportés
data Format = XML | JSON | Markdown deriving (Show, Eq)

-- Détecter le format d'un document à partir de son contenu
detectFormat :: String -> Maybe Format
detectFormat input = 
  let trimmed = dropWhile isSpace input
  in if "<document>" `isPrefixOf` trimmed || "<?xml" `isPrefixOf` trimmed
     then Just XML
     else if "{" `isPrefixOf` trimmed
          then Just JSON
          else if "---" `isPrefixOf` trimmed
               then Just Markdown
               else Nothing

-- Parser en fonction du format détecté
parseByFormat :: Format -> String -> Maybe Document
parseByFormat XML = parseXML
parseByFormat JSON = parseJSON
parseByFormat Markdown = parseMarkdown

-- Détecter et parser automatiquement
parseAuto :: String -> Maybe Document
parseAuto input = do
  format <- detectFormat input
  parseByFormat format input