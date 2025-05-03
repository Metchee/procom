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
import Data.List (isPrefixOf)
import Data.Char (isSpace)

data Format = XML | JSON | Markdown deriving (Show, Eq)

detectFormat :: String -> Maybe Format
detectFormat input =
  let trimmed = dropWhile isSpace input
  in case () of
    _ | "<document>" `isPrefixOf` trimmed ||
      "<?xml" `isPrefixOf` trimmed -> Just XML
      | "{" `isPrefixOf` trimmed -> Just JSON
      | "---" `isPrefixOf` trimmed -> Just Markdown
      | otherwise -> Nothing

parseByFormat :: Format -> String -> Maybe Document
parseByFormat XML = parseXML
parseByFormat JSON = parseJSON
parseByFormat Markdown = parseMarkdown

parseAuto :: String -> Maybe Document
parseAuto input = do
  format <- detectFormat input
  parseByFormat format input
