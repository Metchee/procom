module Formatter.XML where

import Document.Types
import Data.Maybe (fromMaybe)

-- Formatter un document en XML
formatXML :: Document -> String
formatXML doc = 
  "<document>\n" ++
  formatHeader (docHeader doc) ++
  formatBody (docBody doc) ++
  "</document>\n"

-- Formatter l'en-tête en XML
formatHeader :: Header -> String
formatHeader header =
  "  <header title=\"" ++ headerTitle header ++ "\"" ++
  formatOptionalAttr "author" (headerAuthor header) ++
  formatOptionalAttr "date" (headerDate header) ++
  "></header>\n"
  where
    formatOptionalAttr _ Nothing = ""
    formatOptionalAttr name (Just value) = " " ++ name ++ "=\"" ++ value ++ "\""

-- Formatter le corps en XML
formatBody :: [Content] -> String
formatBody contents =
  "  <body>\n" ++
  concatMap (formatContent 4) contents ++
  "  </body>\n"

-- Formatter un contenu en XML avec indentation
formatContent :: Int -> Content -> String
formatContent indent (Text text) = 
  spaces indent ++ escapeXML text ++ "\n"
formatContent indent (Italic content) =
  spaces indent ++ "<italic>" ++ formatContentInline content ++ "</italic>\n"
formatContent indent (Bold content) =
  spaces indent ++ "<bold>" ++ formatContentInline content ++ "</bold>\n"
formatContent indent (Code code) =
  spaces indent ++ "<code>" ++ escapeXML code ++ "</code>\n"
formatContent indent (Link text url) =
  spaces indent ++ "<link text=\"" ++ escapeXML text ++ "\" url=\"" ++ escapeXML url ++ "\"></link>\n"
formatContent indent (Image alt url) =
  spaces indent ++ "<image alt=\"" ++ escapeXML alt ++ "\" url=\"" ++ escapeXML url ++ "\"></image>\n"
formatContent indent (Paragraph contents) =
  spaces indent ++ "<paragraph>\n" ++
  concatMap (formatContent (indent + 2)) contents ++
  spaces indent ++ "</paragraph>\n"
formatContent indent (Section title contents) =
  spaces indent ++ "<section title=\"" ++ escapeXML title ++ "\">\n" ++
  concatMap (formatContent (indent + 2)) contents ++
  spaces indent ++ "</section>\n"
formatContent indent (CodeBlock code) =
  spaces indent ++ "<codeblock>\n" ++
  spaces (indent + 2) ++ escapeXML code ++ "\n" ++
  spaces indent ++ "</codeblock>\n"
formatContent indent (List items) =
  spaces indent ++ "<list>\n" ++
  concatMap (formatItem (indent + 2)) items ++
  spaces indent ++ "</list>\n"

-- Formatter un élément de liste en XML
formatItem :: Int -> Item -> String
formatItem indent (Item contents) =
  spaces indent ++ "<item>\n" ++
  concatMap (formatContent (indent + 2)) contents ++
  spaces indent ++ "</item>\n"

-- Formatter un contenu en ligne (sans indentation ni saut de ligne)
formatContentInline :: Content -> String
formatContentInline (Text text) = escapeXML text
formatContentInline (Italic content) = "<italic>" ++ formatContentInline content ++ "</italic>"
formatContentInline (Bold content) = "<bold>" ++ formatContentInline content ++ "</bold>"
formatContentInline (Code code) = "<code>" ++ escapeXML code ++ "</code>"
formatContentInline (Link text url) = "<link text=\"" ++ escapeXML text ++ "\" url=\"" ++ escapeXML url ++ "\"></link>"
formatContentInline (Image alt url) = "<image alt=\"" ++ escapeXML alt ++ "\" url=\"" ++ escapeXML url ++ "\"></image>"
formatContentInline _ = "" -- Autres cas non supportés en inline

-- Échapper les caractères spéciaux XML
escapeXML :: String -> String
escapeXML = concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&apos;"
    escapeChar c = [c]

-- Fonction utilitaire pour générer des espaces
spaces :: Int -> String
spaces n = replicate n ' '