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
    body = trimEnd (formatBody (docBody doc))
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
formatContent (Italic content) = "*" ++ formatContentInline content ++ "*"
formatContent (Bold content) = "**" ++ formatContentInline content ++ "**"
formatContent (Code code) = "`" ++ code ++ "`"
formatContent (Link text url) = "[" ++ text ++ "](" ++ url ++ ")"
formatContent (Image alt url) = "![" ++ alt ++ "](" ++ url ++ ")"
formatContent (Paragraph contents) = 
  concatMap formatContentInline contents ++ "\n\n"
formatContent (Section title contents) = 
  "# " ++ title ++ "\n\n" ++
  concatMap formatContent contents
formatContent (CodeBlock code) = 
  "```\n" ++ code ++ "\n```\n\n"
formatContent (List items) = 
  concatMap formatItem items ++ "\n"

formatItem :: Item -> String
formatItem (Item contents) = 
  "- " ++ concatMap formatContentInline contents ++ "\n"

formatContentInline :: Content -> String
formatContentInline (Text text) = text
formatContentInline (Italic content) = "*" ++
  formatContentInline content ++ "*"
formatContentInline (Bold content) = "**" ++
  formatContentInline content ++ "**"
formatContentInline (Code code) = "`" ++ code ++ "`"
formatContentInline (Link text url) = "[" ++ text ++ "](" ++ url ++ ")"
formatContentInline (Image alt url) = "![" ++ alt ++ "](" ++ url ++ ")"
formatContentInline (Paragraph contents) =
  concatMap formatContentInline contents
formatContentInline (Section title _) = "# " ++ title
formatContentInline (CodeBlock code) = "```" ++ code ++ "```"
formatContentInline (List items) = concatMap formatItemInline items

formatItemInline :: Item -> String
formatItemInline (Item contents) =
  "- " ++ concatMap formatContentInline contents