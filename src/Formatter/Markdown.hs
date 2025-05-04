{-
-- EPITECH PROJECT, 2025
-- procom
-- File description:
-- Markdown
-}

module Formatter.Markdown where

import Document.Types
import Data.Maybe (fromMaybe)

formatMarkdown :: Document -> String
formatMarkdown doc =
  let
    header = formatHeader (docHeader doc)
    body = trimEnd (formatBody (docContent doc))
  in header ++ body

trimEnd :: String -> String
trimEnd = reverse . dropWhile (== '\n') . reverse

formatHeader :: Header -> String
formatHeader header =
  "---\n" ++
  "title: " ++ headerTitle header ++ "\n" ++
  formatOptionalField "author" (headerAuthor header) ++
  formatOptionalField "date" (headerDate header) ++
  "---\n\n"
  where
    formatOptionalField _ Nothing = ""
    formatOptionalField name (Just value) = name ++ ": " ++ value ++ "\n"

formatBody :: [Content] -> String
formatBody = concatMap formatContent

formatContent :: Content -> String
formatContent (Text text) = text ++ "\n\n"
formatContent (Italic content) = "*" ++ formatContentInline content ++ "*\n\n"
formatContent (Bold content) = "**" ++ formatContentInline content ++ "**\n\n"
formatContent (Code code) = "`" ++ code ++ "`\n\n"
formatContent (Link text url) = "[" ++ text ++ "](" ++ url ++ ")\n\n"
formatContent (Image alt url) = "![" ++ alt ++ "](" ++ url ++ ")\n\n"
formatContent (Paragraph contents) = 
  concatMap formatParagraphContent contents ++ "\n\n"
formatContent (Section titleMaybe contents) = 
  "# " ++ maybe "" id titleMaybe ++ "\n\n" ++
  concatMap formatContent contents
formatContent (CodeBlock contents) = 
  "```\n" ++ concatMap formatContentInlineNoWrap contents ++ "\n```\n\n"
formatContent (List items) = 
  concatMap formatItem items ++ "\n"

formatParagraphContent :: Content -> String
formatParagraphContent (Italic content) = 
  "*" ++ formatContentInline content ++ "*"
formatParagraphContent (Bold content) = 
  "**" ++ formatContentInline content ++ "**"
formatParagraphContent (Code code) = "`" ++ code ++ "`"
formatParagraphContent (Link text url) = 
  "[" ++ text ++ "](" ++ url ++ ")"
formatParagraphContent (Image alt url) = 
  "![" ++ alt ++ "](" ++ url ++ ")"
formatParagraphContent (Text text) = text
formatParagraphContent (Paragraph contents) =
  concatMap formatParagraphContent contents
formatParagraphContent _ = ""

formatItem :: Item -> String
formatItem (Item contents) = 
  "- " ++ concatMap formatContentInline contents ++ "\n"

formatContentInline :: Content -> String
formatContentInline (Text text) = text
formatContentInline (Italic content) = 
  "*" ++ formatContentInline content ++ "*"
formatContentInline (Bold content) = 
  "**" ++ formatContentInline content ++ "**"
formatContentInline (Code code) = "`" ++ code ++ "`"
formatContentInline (Link text url) = 
  "[" ++ text ++ "](" ++ url ++ ")"
formatContentInline (Image alt url) = 
  "![" ++ alt ++ "](" ++ url ++ ")"
formatContentInline (Paragraph contents) =
  concatMap formatContentInline contents
formatContentInline (Section titleMaybe _) = 
  "# " ++ maybe "" id titleMaybe
formatContentInline (CodeBlock contents) = 
  "```" ++ concatMap formatContentInline contents ++ "```"
formatContentInline (List items) = 
  concatMap formatItemInline items

formatContentInlineNoWrap :: Content -> String
formatContentInlineNoWrap (Text text) = text
formatContentInlineNoWrap (Paragraph contents) = 
  concatMap formatContentInlineNoWrap contents
formatContentInlineNoWrap _ = ""

formatItemInline :: Item -> String
formatItemInline (Item contents) =
  "- " ++ concatMap formatContentInline contents

cleanSpaces :: String -> String
cleanSpaces = unwords . words