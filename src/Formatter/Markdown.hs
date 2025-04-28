module Formatter.Markdown where

import Document.Types
import Data.Maybe (fromMaybe)

-- Formatter un document en Markdown
formatMarkdown :: Document -> String
formatMarkdown doc = 
  formatHeader (docHeader doc) ++
  "\n" ++
  formatBody (docBody doc)

-- Formatter l'en-tête en Markdown (YAML frontmatter)
formatHeader :: Header -> String
formatHeader header =
  "---\n" ++
  "title: " ++ headerTitle header ++ "\n" ++
  formatOptionalField "author" (headerAuthor header) ++
  formatOptionalField "date" (headerDate header) ++
  "---\n"
  where
    formatOptionalField _ Nothing = ""
    formatOptionalField name (Just value) = name ++ ": " ++ value ++ "\n"

-- Formatter le corps en Markdown
formatBody :: [Content] -> String
formatBody = concatMap formatContent

-- Formatter un contenu en Markdown
formatContent :: Content -> String
formatContent (Text text) = text ++ "\n"
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

-- Formatter un élément de liste en Markdown
formatItem :: Item -> String
formatItem (Item contents) = 
  "- " ++ concatMap formatContentInline contents ++ "\n"

-- Formatter un contenu en ligne pour Markdown
formatContentInline :: Content -> String
formatContentInline (Text text) = text
formatContentInline (Italic content) = "*" ++ formatContentInline content ++ "*"
formatContentInline (Bold content) = "**" ++ formatContentInline content ++ "**"
formatContentInline (Code code) = "`" ++ code ++ "`"
formatContentInline (Link text url) = "[" ++ text ++ "](" ++ url ++ ")"
formatContentInline (Image alt url) = "![" ++ alt ++ "](" ++ url ++ ")"
formatContentInline _ = "" -- Autres cas non supportés en inline