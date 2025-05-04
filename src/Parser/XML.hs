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

-- Parse an XML attribute
parseAttribute :: Parser (String, String)
parseAttribute = do
  spaces
  key <- some (satisfy (\c -> isAlphaNum c || c == '_' || c == '-'))
  spaces
  char '='
  spaces
  quote <- char '"' <|> char '\''
  value <- many (satisfy (/= quote))
  char quote
  spaces
  return (key, value)

-- Parse the opening tag of an XML element
parseOpenTag :: String -> Parser [(String, String)]
parseOpenTag tag = do
  spaces
  string "<"
  spaces
  string tag
  spaces
  attrs <- many parseAttribute
  spaces
  string ">"
  spaces
  return attrs

-- Parse the closing tag of an XML element
parseCloseTag :: String -> Parser ()
parseCloseTag tag = do
  spaces
  string "</"
  spaces
  string tag
  spaces
  string ">"
  spaces
  return ()

-- Parse an XML element with its content
parseElement :: String -> Parser a -> Parser ([(String, String)], a)
parseElement tag contentParser = do
  attrs <- parseOpenTag tag
  content <- contentParser
  parseCloseTag tag
  return (attrs, content)

-- Parse a self-closing XML element
parseSelfClosingTag :: String -> Parser [(String, String)]
parseSelfClosingTag tag = do
  spaces
  string "<"
  spaces
  string tag
  spaces
  attrs <- many parseAttribute
  spaces
  string "/>"
  spaces
  return attrs

-- Parse an XML element that can be either normal or self-closing
parseElementOrSelfClosing :: String -> Parser a -> Parser ([(String, String)], Maybe a)
parseElementOrSelfClosing tag contentParser = 
  (parseElement tag contentParser >>= \(attrs, content) -> return (attrs, Just content))
  <|>
  (parseSelfClosingTag tag >>= \attrs -> return (attrs, Nothing))

-- Parse the entire XML document
parseXMLDocument :: Parser Document
parseXMLDocument = do
  spaces
  _ <- optional (parseXMLDeclaration)
  spaces
  _ <- parseOpenTag "document"
  header <- parseXMLHeader
  body <- parseXMLBody
  parseCloseTag "document"
  return (Document header body)

-- Parse XML declaration (optional)
parseXMLDeclaration :: Parser ()
parseXMLDeclaration = do
  string "<?xml"
  many (satisfy (/= '>'))
  string "?>"
  return ()

-- Parse the header section of an XML document
parseXMLHeader :: Parser Header
parseXMLHeader = do
  (attrs, _) <- parseElementOrSelfClosing "header" (return ())
  return $ Header 
    (fromMaybe "" (lookup "title" attrs))
    (lookup "author" attrs)
    (lookup "date" attrs)

-- Parse the body section of an XML document
parseXMLBody :: Parser [Content]
parseXMLBody = do
  (_, content) <- parseElement "body" (many parseXMLContent)
  return content

-- Parse any XML content element
parseXMLContent :: Parser Content
parseXMLContent = choice
  [ parseXMLParagraph
  , parseXMLSection
  , parseXMLText
  , parseXMLBold
  , parseXMLItalic
  , parseXMLCode
  , parseXMLCodeBlock
  , parseXMLLink
  , parseXMLImage
  , parseXMLList
  ]

-- Parse a text node
parseXMLText :: Parser Content
parseXMLText = do
  spaces
  text <- some (satisfy (\c -> c /= '<' && c /= '>'))
  spaces
  return (Text text)

-- Parse a paragraph element
parseXMLParagraph :: Parser Content
parseXMLParagraph = do
  (_, content) <- parseElement "paragraph" (many parseXMLContent)
  return (Paragraph content)

-- Parse a section element
parseXMLSection :: Parser Content
parseXMLSection = do
  (attrs, content) <- parseElement "section" (many parseXMLContent)
  let title = fromMaybe "" (lookup "title" attrs)
  return (Section title content)

-- Parse a bold element
parseXMLBold :: Parser Content
parseXMLBold = do
  (_, content) <- parseElement "bold" (choice [parseXMLText, parseXMLItalic, parseXMLCode])
  return (Bold content)

-- Parse an italic element
parseXMLItalic :: Parser Content
parseXMLItalic = do
  (_, content) <- parseElement "italic" (choice [parseXMLText, parseXMLBold, parseXMLCode])
  return (Italic content)

-- Parse a code element
parseXMLCode :: Parser Content
parseXMLCode = do
  (_, content) <- parseElement "code" (Text <$> many (satisfy (/= '<')))
  return $ case content of
    Text t -> Code t
    _ -> Code ""

-- Parse a code block element
parseXMLCodeBlock :: Parser Content
parseXMLCodeBlock = do
  (_, content) <- parseElement "codeblock" (Text <$> many (satisfy (/= '<')))
  return $ case content of
    Text t -> CodeBlock t
    _ -> CodeBlock ""

-- Parse a link element
parseXMLLink :: Parser Content
parseXMLLink = do
  (attrs, _) <- parseElementOrSelfClosing "link" (return ())
  let text = fromMaybe "" (lookup "text" attrs)
  let url = fromMaybe "" (lookup "url" attrs)
  return (Link text url)

-- Parse an image element
parseXMLImage :: Parser Content
parseXMLImage = do
  (attrs, _) <- parseElementOrSelfClosing "image" (return ())
  let alt = fromMaybe "" (lookup "alt" attrs)
  let url = fromMaybe "" (lookup "url" attrs)
  return (Image alt url)

-- Parse a list element
parseXMLList :: Parser Content
parseXMLList = do
  (_, items) <- parseElement "list" (many parseXMLItem)
  return (List items)

-- Parse a list item
parseXMLItem :: Parser Item
parseXMLItem = do
  (_, content) <- parseElement "item" (many parseXMLContent)
  return (Item content)

-- Helper function to choose between multiple parsers
choice :: [Parser a] -> Parser a
choice [] = Parser $ \_ -> Nothing
choice (p:ps) = p <|> choice ps

-- Main entry point for parsing XML content
parseXML :: String -> Maybe Document
parseXML input = case runParser parseXMLDocument input of
  Just (doc, rest) | all isSpace rest -> Just doc
  _ -> Nothing