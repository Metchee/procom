{-
-- EPITECH PROJECT, 2025
-- procom
-- File description:
-- Detect
-}

module Parser.Detect where

import Document.Types
import Parser.XML (parseXML)
import Parser.JSON (parseJSON)
import Parser.Markdown (parseMarkdown)
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)

data Format = XML | JSON | Markdown deriving (Show, Eq)

-- Détection de format plus souple
detectFormat :: String -> Maybe Format
detectFormat input =
  let trimmed = dropWhile isSpace input
  in case () of
    _ | any (\tag -> tag `isInfixOf` trimmed) ["<document", "<?xml", "</document>", "<header"] -> Just XML
      | any (\tag -> tag `isPrefixOf` trimmed) ["{", "{\""] || "\"header\"" `isInfixOf` trimmed -> Just JSON
      | "---" `isPrefixOf` trimmed 
        || "title:" `isInfixOf` trimmed
        || (not (null trimmed) && all isSpace trimmed) -> Just Markdown
      | otherwise -> Just XML  -- Par défaut, on essaie XML comme dernier recours

parseByFormat :: Format -> String -> Maybe Document
parseByFormat XML = parseXML
parseByFormat JSON = parseJSON
parseByFormat Markdown = parseMarkdown

-- Essaie tous les parseurs si le format n'est pas spécifié
parseAuto :: String -> Maybe Document
parseAuto input = 
  let tryParse format = parseByFormat format input
      results = filter isJust [tryParse XML, tryParse JSON, tryParse Markdown]
  in if null results then Nothing else head results
  where 
    isJust (Just _) = True
    isJust Nothing = False