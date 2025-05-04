{-
-- EPITECH PROJECT, 2025
-- procom
-- File description:
-- XML
-}

module Parser.XML where

import Parser.Core
import Document.Types
import Control.Applicative (many, some, (<|>), optional)
import Data.Char (isSpace, isAlpha, isAlphaNum)
import Data.Maybe (catMaybes, fromMaybe)

-- Simple XML parser that focuses on correctly extracting document structure
parseXML :: String -> Maybe Document
parseXML input = 
  case runParser parseDocument input of
    Just (doc, rest) | all isSpace rest -> Just doc
    _ -> Nothing

parseDocument :: Parser Document
parseDocument = do
  spaces
  _ <- string "<document>"
  spaces
  header <- parseHeader
  spaces
  body <- parseBody
  spaces
  _ <- string "</document>"
  spaces
  return $ Document header body

parseHeader :: Parser Header
parseHeader = do
  spaces
  _ <- string "<header"
  spaces
  attrs <- many parseAttribute
  let title = fromMaybe "" (lookup "title" attrs)
  let author = lookup "author" attrs
  let date = lookup "date" attrs
  
  -- Check if this is a self-closing header or one with content
  isClosing <- peek (string "/>")
  
  if isClosing
    then string "/>" >> spaces >> return (Header title author date)
    else do
      string ">"
      spaces
      mAuthor <- optional parseAuthorElem
      mDate <- optional parseDateElem
      spaces
      _ <- string "</header>"
      spaces
      let finalAuthor = mAuthor <|> author
      let finalDate = mDate <|> date
      return $ Header title finalAuthor finalDate

parseAuthorElem :: Parser String
parseAuthorElem = do
  spaces
  _ <- string "<author>"
  content <- many (satisfy (/= '<'))
  _ <- string "</author>"
  spaces
  return content

parseDateElem :: Parser String
parseDateElem = do
  spaces
  _ <- string "<date>"
  content <- many (satisfy (/= '<'))
  _ <- string "</date>"
  spaces
  return content

parseAttribute :: Parser (String, String)
parseAttribute = do
  spaces
  name <- some (satisfy (\c -> isAlphaNum c || c == '_' || c == '-'))
  spaces
  _ <- string "="
  spaces
  quote <- char '"' <|> char '\''
  value <- many (satisfy (/= quote))
  _ <- char quote
  spaces
  return (name, value)

parseBody :: Parser [Content]
parseBody = do
  spaces
  _ <- string "<body>"
  spaces
  contents <- many parseContent
  spaces
  _ <- string "</body>"
  spaces
  return contents

parseContent :: Parser Content
parseContent = choice [
    parseParagraph,
    parseSection,
    parseCodeBlock,
    parseList,
    parseBold,
    parseItalic,
    parseCode,
    parseLink,
    parseImage,
    parseText
  ]

parseParagraph :: Parser Content
parseParagraph = do
  spaces
  _ <- string "<paragraph>"
  spaces
  contents <- many parseContent
  spaces
  _ <- string "</paragraph>"
  spaces
  return $ Paragraph contents

parseSection :: Parser Content
parseSection = do
  spaces
  _ <- string "<section"
  spaces
  attrs <- many parseAttribute
  let title = lookup "title" attrs
  spaces
  _ <- string ">"
  spaces
  contents <- many parseContent
  spaces
  _ <- string "</section>"
  spaces
  return $ Section title contents

parseCodeBlock :: Parser Content
parseCodeBlock = do
  spaces
  _ <- string "<codeblock>"
  spaces
  contents <- many parseContent
  spaces
  _ <- string "</codeblock>"
  spaces
  return $ CodeBlock contents

parseList :: Parser Content
parseList = do
  spaces
  _ <- string "<list>"
  spaces
  items <- many parseListItem
  spaces
  _ <- string "</list>"
  spaces
  return $ List items

parseListItem :: Parser Item
parseListItem = do
  spaces
  -- Handle both directly nested content and paragraph-wrapped content
  content <- parseParagraphAsItem <|> parseDirectItem
  spaces
  return content

parseParagraphAsItem :: Parser Item
parseParagraphAsItem = do
  para <- parseParagraph
  return $ Item [para]

parseDirectItem :: Parser Item
parseDirectItem = do
  _ <- string "<item>"
  spaces
  contents <- many parseContent
  spaces
  _ <- string "</item>"
  spaces
  return $ Item contents

parseBold :: Parser Content
parseBold = do
  spaces
  _ <- string "<bold>"
  spaces
  contents <- many parseContent
  spaces
  _ <- string "</bold>"
  spaces
  let content = case contents of
                  [single] -> single
                  _ -> Text (concatMap contentToText contents)
  return $ Bold content

parseItalic :: Parser Content
parseItalic = do
  spaces
  _ <- string "<italic>"
  spaces
  contents <- many parseContent
  spaces
  _ <- string "</italic>"
  spaces
  let content = case contents of
                  [single] -> single
                  _ -> Text (concatMap contentToText contents)
  return $ Italic content

parseCode :: Parser Content
parseCode = do
  spaces
  _ <- string "<code>"
  spaces
  content <- many (satisfy (/= '<'))
  spaces
  _ <- string "</code>"
  spaces
  return $ Code content

parseLink :: Parser Content
parseLink = do
  spaces
  _ <- string "<link"
  spaces
  attrs <- many parseAttribute
  let text = fromMaybe "link" (lookup "text" attrs)
  let url = fromMaybe "" (lookup "url" attrs)
  
  -- Handle both self-closing and content forms
  isSelfClosing <- peek (string "/>")
  if isSelfClosing
    then string "/>" >> spaces >> return (Link text url)
    else do
      string ">"
      spaces
      content <- many (satisfy (/= '<'))
      _ <- string "</link>"
      spaces
      let finalText = if null content then text else content
      return $ Link finalText url

parseImage :: Parser Content
parseImage = do
  spaces
  _ <- string "<image"
  spaces
  attrs <- many parseAttribute
  let alt = fromMaybe "image" (lookup "alt" attrs)
  let url = fromMaybe "" (lookup "url" attrs)
  
  -- Handle both self-closing and content forms
  isSelfClosing <- peek (string "/>")
  if isSelfClosing
    then string "/>" >> spaces >> return (Image alt url)
    else do
      string ">"
      spaces
      content <- many (satisfy (/= '<'))
      _ <- string "</image>"
      spaces
      let finalAlt = if null content then alt else content
      return $ Image finalAlt url

parseText :: Parser Content
parseText = do
  spaces
  text <- some (satisfy (\c -> c /= '<' && c /= '>'))
  spaces
  return $ Text text

contentToText :: Content -> String
contentToText (Text t) = t
contentToText _ = ""

choice :: [Parser a] -> Parser a
choice [] = Parser $ \_ -> Nothing
choice (p:ps) = p <|> choice ps