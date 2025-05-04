{-
-- EPITECH PROJECT, 2025
-- procom
-- File description:
-- XML
-}

module Formatter.XML where

import Document.Types
import Data.Maybe (fromMaybe)

formatXML :: Document -> String
formatXML doc = 
  "<document>\n" ++
  formatHeader (docHeader doc) ++
  "  <body>\n" ++
  concatMap (formatContent 4) (docContent doc) ++
  "  </body>\n" ++
  "</document>\n"

formatHeader :: Header -> String
formatHeader header =
  "  <header title=\"" ++ escapeXML (headerTitle header) ++ "\"" ++
  formatOptionalAttr "author" (headerAuthor header) ++
  formatOptionalAttr "date" (headerDate header) ++
  "></header>\n"
  where
    formatOptionalAttr _ Nothing = ""
    formatOptionalAttr name (Just value) = " " ++
      name ++ "=\"" ++ escapeXML value ++ "\""

formatContent :: Int -> Content -> String
formatContent indent (Text text) = 
  spaces indent ++ escapeXML text ++ "\n"
formatContent indent (Italic content) =
  spaces indent ++ "<italic>" ++
  formatContentInline content ++ "</italic>\n"
formatContent indent (Bold content) =
  spaces indent ++ "<bold>" ++
  formatContentInline content ++ "</bold>\n"
formatContent indent (Code code) =
  spaces indent ++ "<code>" ++
  escapeXML code ++ "</code>\n"
formatContent indent (Link text url) =
  spaces indent ++ "<link text=\"" ++
  escapeXML text ++ "\" url=\"" ++ escapeXML url ++ "\"></link>\n"
formatContent indent (Image alt url) =
  spaces indent ++ "<image alt=\"" ++
  escapeXML alt ++ "\" url=\"" ++ escapeXML url ++ "\"></image>\n"
formatContent indent (Paragraph contents) =
  spaces indent ++ "<paragraph>\n" ++
  concatMap (formatContent (indent + 2)) contents ++
  spaces indent ++ "</paragraph>\n"
formatContent indent (Section titleMaybe contents) =
  let title = maybe "" id titleMaybe in
  spaces indent ++ "<section title=\"" ++ escapeXML title ++ "\">\n" ++
  concatMap (formatContent (indent + 2)) contents ++
  spaces indent ++ "</section>\n"
formatContent indent (CodeBlock contents) =
  spaces indent ++ "<codeblock>\n" ++
  concatMap (formatContent (indent + 2)) contents ++
  spaces indent ++ "</codeblock>\n"
formatContent indent (List items) =
  spaces indent ++ "<list>\n" ++
  concatMap (formatItem (indent + 2)) items ++
  spaces indent ++ "</list>\n"

formatItem :: Int -> Item -> String
formatItem indent (Item contents) =
  spaces indent ++ "<item>\n" ++
  concatMap (formatContent (indent + 2)) contents ++
  spaces indent ++ "</item>\n"

formatContentInline :: Content -> String
formatContentInline (Text text) = escapeXML text
formatContentInline (Italic content) = "<italic>" ++
  formatContentInline content ++ "</italic>"
formatContentInline (Bold content) = "<bold>" ++
  formatContentInline content ++ "</bold>"
formatContentInline (Code code) = "<code>" ++ escapeXML code ++ "</code>"
formatContentInline (Link text url) = "<link text=\"" ++
  escapeXML text ++ "\" url=\"" ++ escapeXML url ++ "\"></link>"
formatContentInline (Image alt url) = "<image alt=\"" ++
  escapeXML alt ++ "\" url=\"" ++ escapeXML url ++ "\"></image>"
formatContentInline (Paragraph contents) =
  concatMap formatContentInline contents
formatContentInline (Section titleMaybe contents) =
  let title = maybe "" id titleMaybe in
  "<section title=\"" ++ escapeXML title ++ "\">" ++
  concatMap formatContentInline contents ++ "</section>"
formatContentInline (CodeBlock contents) =
  "<codeblock>" ++ concatMap formatContentInline contents ++ "</codeblock>"
formatContentInline (List items) =
  "<list>" ++ concatMap formatItemInline items ++ "</list>"

formatItemInline :: Item -> String
formatItemInline (Item contents) =
  "<item>" ++ concatMap formatContentInline contents ++ "</item>"

escapeXML :: String -> String
escapeXML = concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&apos;"
    escapeChar c = [c]

spaces :: Int -> String
spaces n = replicate n ' '