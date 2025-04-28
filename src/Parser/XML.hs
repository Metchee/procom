module Parser.XML where

import Parser.Core
import Document.Types
import Control.Applicative (many, some, (<|>))
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Char (isSpace, isAlpha)
-- Parser pour les balises XML
openTag :: String -> Parser String
openTag name = do
  string "<"
  spaces
  string name
  spaces
  attributes <- many attribute
  spaces
  string ">"
  return name

closeTag :: String -> Parser String
closeTag name = do
  string "</"
  spaces
  string name
  spaces
  string ">"
  return name

-- Parser pour les attributs
attribute :: Parser (String, String)
attribute = do
  spaces
  name <- some (satisfy (\c -> isAlpha c || c == '_'))
  spaces
  char '='
  spaces
  value <- quotedString
  return (name, value)

-- Parser pour les éléments XML
element :: Parser (String, [(String, String)], String)
element = do
  string "<"
  spaces
  tagName <- some (satisfy (\c -> isAlpha c || c == '_'))
  spaces
  attrs <- many attribute
  spaces
  string ">"
  content <- tillString ("</" ++ tagName)
  closeTag tagName
  return (tagName, attrs, content)

-- Parser pour le document XML
parseXMLDocument :: Parser Document
parseXMLDocument = do
  openTag "document"
  header <- parseXMLHeader
  body <- parseXMLBody
  closeTag "document"
  return $ Document header body

-- Parser pour l'en-tête XML
parseXMLHeader :: Parser Header
parseXMLHeader = do
  openTag "header"
  title <- parseAttribute "title"
  author <- optionalAttribute "author"
  date <- optionalAttribute "date"
  closeTag "header"
  return $ Header title author date
  where
    parseAttribute name = do
      string "<"
      spaces
      string name
      spaces
      char '='
      spaces
      value <- quotedString
      spaces
      string "/>"
      return value
    
    optionalAttribute name = do
      hasAttr <- peek (string ("<" ++ name))
      if hasAttr
        then Just <$> parseAttribute name
        else return Nothing

-- Parser pour le corps XML
parseXMLBody :: Parser [Content]
parseXMLBody = do
  openTag "body"
  contents <- many parseXMLContent
  closeTag "body"
  return contents

-- Parser pour les contenus XML
parseXMLContent :: Parser Content
parseXMLContent = parseParagraph <|> parseSection <|> parseText <|> parseList
  where
    parseParagraph = do
      openTag "paragraph"
      contents <- many parseXMLContent
      closeTag "paragraph"
      return $ Paragraph contents
    
    parseSection = do
      string "<section"
      spaces
      title <- optionalTitle
      string ">"
      contents <- many parseXMLContent
      closeTag "section"
      return $ Section (maybe "" id title) contents
      where
        optionalTitle = do
          hasTitle <- peek (string "title=")
          if hasTitle
            then do
              string "title="
              Just <$> quotedString
            else return Nothing
    
    parseText = do
      text <- tillString "<"
      return $ Text text
    
    parseList = do
      openTag "list"
      items <- many parseItem
      closeTag "list"
      return $ List items
    
    parseItem = do
      openTag "item"
      contents <- many parseXMLContent
      closeTag "item"
      return $ Item contents

-- Parse un document XML à partir d'une chaîne
parseXML :: String -> Maybe Document
parseXML input = case runParser parseXMLDocument input of
  Just (doc, "") -> Just doc
  Just (doc, rest) | all isSpace rest -> Just doc
  _ -> Nothing