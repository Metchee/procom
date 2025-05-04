{-
-- EPITECH PROJECT, 2025
-- procom
-- File description:
-- JSON
-}

module Formatter.JSON where

import Document.Types
import Data.Maybe (fromMaybe)

formatJSON :: Document -> String
formatJSON doc =
  "{\n" ++
  "  \"header\": " ++ formatHeader (docHeader doc) ++ ",\n" ++
  "  \"body\": " ++ formatBody (docContent doc) ++ "\n" ++
  "}\n"

formatHeader :: Header -> String
formatHeader header =
  "{\n" ++
  "    \"title\": \"" ++ escape (headerTitle header) ++ "\"" ++
  formatOptionalField "author" (headerAuthor header) ++
  formatOptionalField "date" (headerDate header) ++
  "\n  }"
  where
    formatOptionalField _ Nothing = ""
    formatOptionalField name (Just value) =
      ",\n    \"" ++ name ++ "\": \"" ++ escape value ++ "\""

formatBody :: [Content] -> String
formatBody contents =
  "[\n" ++
  joinWithComma (map (formatContent 4) contents) ++
  "\n  ]"

formatContent :: Int -> Content -> String
formatContent indent (Text text) =
  spaces indent ++ "\"" ++ escape text ++ "\""
formatContent indent (Italic content) =
  spaces indent ++ "{\n" ++
  spaces (indent + 2) ++ "\"type\": \"italic\",\n" ++
  spaces (indent + 2) ++ "\"content\": " ++
  formatContentInline content ++ "\n" ++
  spaces indent ++ "}"
formatContent indent (Bold content) =
  spaces indent ++ "{\n" ++
  spaces (indent + 2) ++ "\"type\": \"bold\",\n" ++
  spaces (indent + 2) ++ "\"content\": " ++
  formatContentInline content ++ "\n" ++
  spaces indent ++ "}"
formatContent indent (Code code) =
  spaces indent ++ "{\n" ++
  spaces (indent + 2) ++ "\"type\": \"code\",\n" ++
  spaces (indent + 2) ++ "\"content\": \"" ++ escape code ++ "\"\n" ++
  spaces indent ++ "}"
formatContent indent (Link text url) =
  spaces indent ++ "{\n" ++
  spaces (indent + 2) ++ "\"type\": \"link\",\n" ++
  spaces (indent + 2) ++ "\"text\": \"" ++ escape text ++ "\",\n" ++
  spaces (indent + 2) ++ "\"url\": \"" ++ escape url ++ "\"\n" ++
  spaces indent ++ "}"
formatContent indent (Image alt url) =
  spaces indent ++ "{\n" ++
  spaces (indent + 2) ++ "\"type\": \"image\",\n" ++
  spaces (indent + 2) ++ "\"alt\": \"" ++ escape alt ++ "\",\n" ++
  spaces (indent + 2) ++ "\"url\": \"" ++ escape url ++ "\"\n" ++
  spaces indent ++ "}"
formatContent indent (Paragraph contents) =
  spaces indent ++ "{\n" ++
  spaces (indent + 2) ++ "\"type\": \"paragraph\",\n" ++
  spaces (indent + 2) ++ "\"content\": [\n" ++
  joinWithComma (map (formatContent (indent + 4)) contents) ++ "\n" ++
  spaces (indent + 2) ++ "]\n" ++
  spaces indent ++ "}"
formatContent indent (Section titleMaybe contents) =
  let title = maybe "" id titleMaybe in
  spaces indent ++ "{\n" ++
  spaces (indent + 2) ++ "\"type\": \"section\",\n" ++
  spaces (indent + 2) ++ "\"title\": \"" ++ escape title ++ "\",\n" ++
  spaces (indent + 2) ++ "\"content\": [\n" ++
  joinWithComma (map (formatContent (indent + 4)) contents) ++ "\n" ++
  spaces (indent + 2) ++ "]\n" ++
  spaces indent ++ "}"
formatContent indent (CodeBlock contents) =
  spaces indent ++ "{\n" ++
  spaces (indent + 2) ++ "\"type\": \"codeblock\",\n" ++
  spaces (indent + 2) ++ "\"content\": [\n" ++
  joinWithComma (map (formatContent (indent + 4)) contents) ++ "\n" ++
  spaces (indent + 2) ++ "]\n" ++
  spaces indent ++ "}"
formatContent indent (List items) =
  spaces indent ++ "{\n" ++
  spaces (indent + 2) ++ "\"type\": \"list\",\n" ++
  spaces (indent + 2) ++ "\"items\": [\n" ++
  joinWithComma (map (formatItem (indent + 4)) items) ++ "\n" ++
  spaces (indent + 2) ++ "]\n" ++
  spaces indent ++ "}"

formatItem :: Int -> Item -> String
formatItem indent (Item contents) =
  spaces indent ++ "{\n" ++
  spaces (indent + 2) ++ "\"content\": [\n" ++
  joinWithComma (map (formatContent (indent + 4)) contents) ++ "\n" ++
  spaces (indent + 2) ++ "]\n" ++
  spaces indent ++ "}"

formatContentInline :: Content -> String
formatContentInline (Text text) = "\"" ++ escape text ++ "\""
formatContentInline (Italic content) = 
  "{\n      \"type\": \"italic\",\n      \"content\": " ++
  formatContentInline content ++ "\n    }"
formatContentInline (Bold content) = 
  "{\n      \"type\": \"bold\",\n      \"content\": " ++
  formatContentInline content ++ "\n    }"
formatContentInline (Code code) = 
  "{\n      \"type\": \"code\",\n      \"content\": \"" ++
  escape code ++ "\"\n    }"
formatContentInline (Link text url) = 
  "{\n      \"type\": \"link\",\n      \"text\": \"" ++
  escape text ++ "\",\n      \"url\": \"" ++ escape url ++ "\"\n    }"
formatContentInline (Image alt url) = 
  "{\n      \"type\": \"image\",\n      \"alt\": \"" ++
  escape alt ++ "\",\n      \"url\": \"" ++ escape url ++ "\"\n    }"
formatContentInline (Paragraph contents) =
  "{\n      \"type\": \"paragraph\",\n      \"content\": [" ++ 
  (joinWithComma (map formatContentInline contents)) ++ 
  "]\n    }"
formatContentInline (Section titleMaybe contents) =
  let title = maybe "" id titleMaybe in
  "{\n      \"type\": \"section\",\n      \"title\": \"" ++ escape title ++ 
  "\",\n      \"content\": [" ++ 
  (joinWithComma (map formatContentInline contents)) ++
  "]\n    }"
formatContentInline (CodeBlock contents) =
  "{\n      \"type\": \"codeblock\",\n      \"content\": [" ++
  (joinWithComma (map formatContentInline contents)) ++
  "]\n    }"
formatContentInline (List items) =
  "{\n      \"type\": \"list\",\n      \"items\": [" ++
  (joinWithComma (map formatItemInline items)) ++
  "]\n    }"

formatItemInline :: Item -> String
formatItemInline (Item contents) =
  "{\n        \"content\": [" ++
  (joinWithComma (map formatContentInline contents)) ++
  "]\n      }"

escape :: String -> String
escape = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = [c]

spaces :: Int -> String
spaces n = replicate n ' '

joinWithComma :: [String] -> String
joinWithComma [] = ""
joinWithComma [x] = x
joinWithComma (x:xs) = x ++ ",\n" ++ joinWithComma xs