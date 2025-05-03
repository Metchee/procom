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

detectFormat :: String -> Maybe Format
detectFormat input =
  let trimmed = dropWhile isSpace input
  in detectFormatTrimmed trimmed

detectFormatTrimmed :: String -> Maybe Format
detectFormatTrimmed trimmed
  | isXMLFormat trimmed = Just XML
  | isJSONFormat trimmed = Just JSON
  | isMarkdownFormat trimmed = Just Markdown
  | otherwise = Just XML

isXMLFormat :: String -> Bool
isXMLFormat text = 
  any (\tag -> tag `isInfixOf` text) 
      ["<document", "<?xml", "</document>", "<header"]

isJSONFormat :: String -> Bool
isJSONFormat text =
  any (\tag -> tag `isPrefixOf` text) ["{", "{\""] || 
  "\"header\"" `isInfixOf` text

isMarkdownFormat :: String -> Bool
isMarkdownFormat text =
  "---" `isPrefixOf` text ||
  "title:" `isInfixOf` text ||
  (not (null text) && all isSpace text)

parseByFormat :: Format -> String -> Maybe Document
parseByFormat XML = parseXML
parseByFormat JSON = parseJSON
parseByFormat Markdown = parseMarkdown

parseAuto :: String -> Maybe Document
parseAuto input = 
  let tryParse format = parseByFormat format input
      results = filter isJust [tryParse XML, tryParse JSON, tryParse Markdown]
  in if null results then Nothing else head results
  where 
    isJust (Just _) = True
    isJust Nothing = False