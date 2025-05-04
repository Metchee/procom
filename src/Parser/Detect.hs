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
  | otherwise = Just XML  -- Default to XML if no format is detected

isXMLFormat :: String -> Bool
isXMLFormat text = 
  (take 1 (dropWhile isSpace text) == "<") &&
  any (\tag -> tag `isInfixOf` text) 
      ["<document", "<?xml", "</document>", "<header", "<body>"]

isJSONFormat :: String -> Bool
isJSONFormat text =
  (take 1 (dropWhile isSpace text) == "{") &&
  any (\tag -> tag `isInfixOf` text) 
      ["\"header\"", "\"body\"", "\"title\""]

isMarkdownFormat :: String -> Bool
isMarkdownFormat text =
  "---" `isPrefixOf` dropWhile isSpace text ||
  any (\tag -> tag `isInfixOf` take 100 text)
      ["title:", "author:", "date:"]

parseByFormat :: Format -> String -> Maybe Document
parseByFormat XML = parseXML
parseByFormat JSON = parseJSON
parseByFormat Markdown = parseMarkdown

parseAuto :: String -> Maybe Document
parseAuto input = 
  case detectFormat input of
    Just format -> parseByFormat format input
    Nothing -> Nothing