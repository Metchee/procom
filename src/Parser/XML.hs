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

parseAttributeKey :: Parser String
parseAttributeKey =
  spaces >>
  some (satisfy (\c -> isAlphaNum c || c == '_' || c == '-'))

parseAttributeValue :: Parser String
parseAttributeValue = do
  spaces
  char '='
  spaces
  quote <- char '"' <|> char '\''
  value <- many (satisfy (/= quote))
  char quote
  return value

parseAttribute :: Parser (String, String)
parseAttribute = do
  key <- parseAttributeKey
  value <- parseAttributeValue
  spaces
  return (key, value)

parseTagStart :: String -> Parser ()
parseTagStart tag =
  spaces >>
  string "<" >>
  spaces >>
  string tag >>
  spaces >>
  return ()

parseTagEnd :: Parser ()
parseTagEnd =
  spaces >>
  string ">" >>
  spaces >>
  return ()

parseOpenTag :: String -> Parser [(String, String)]
parseOpenTag tag = do
  parseTagStart tag
  attrs <- many parseAttribute
  parseTagEnd
  return attrs

parseCloseTag :: String -> Parser ()
parseCloseTag tag =
  spaces >>
  string "</" >>
  spaces >>
  string tag >>
  spaces >>
  string ">" >>
  spaces >>
  return ()

parseElement :: String -> Parser a -> Parser ([(String, String)], a)
parseElement tag contentParser = do
  attrs <- parseOpenTag tag
  content <- contentParser
  parseCloseTag tag
  return (attrs, content)

parseSelfClosingEnd :: Parser ()
parseSelfClosingEnd =
  spaces >>
  string "/>" >>
  spaces >>
  return ()

parseSelfClosingTag :: String -> Parser [(String, String)]
parseSelfClosingTag tag = do
  parseTagStart tag
  attrs <- many parseAttribute
  parseSelfClosingEnd
  return attrs

parseElementWithContent :: String -> Parser a -> 
                          Parser ([(String, String)], Maybe a)
parseElementWithContent tag contentParser =
  parseElement tag contentParser >>= \(attrs, content) ->
  return (attrs, Just content)

parseElementNoContent :: String -> Parser ([(String, String)], Maybe a)
parseElementNoContent tag =
  parseSelfClosingTag tag >>= \attrs ->
  return (attrs, Nothing)

parseElementOrSelfClosing :: String -> Parser a -> 
                            Parser ([(String, String)], Maybe a)
parseElementOrSelfClosing tag contentParser =
  parseElementWithContent tag contentParser <|> parseElementNoContent tag

parseXMLDeclaration :: Parser ()
parseXMLDeclaration =
  string "<?xml" >>
  many (satisfy (/= '>')) >>
  string "?>" >>
  return ()

parseXMLHeader :: Parser Header
parseXMLHeader = do
  (attrs, _) <- parseElementOrSelfClosing "header" (return ())
  return $ Header 
    (fromMaybe "" (lookup "title" attrs))
    (lookup "author" attrs)
    (lookup "date" attrs)

parseXMLBody :: Parser [Content]
parseXMLBody = do
  (_, content) <- parseElement "body" (many parseXMLContent)
  return content

parseDocumentStart :: Parser ()
parseDocumentStart = do
  spaces
  _ <- optional parseXMLDeclaration
  spaces
  _ <- parseOpenTag "document"
  return ()

parseDocumentEnd :: Parser ()
parseDocumentEnd = parseCloseTag "document"

parseXMLDocument :: Parser Document
parseXMLDocument = do
  parseDocumentStart
  header <- parseXMLHeader
  body <- parseXMLBody
  parseDocumentEnd
  return (Document header body)

parseXMLBlockContent :: Parser Content
parseXMLBlockContent = choice
  [ parseXMLParagraph
  , parseXMLSection
  , parseXMLCodeBlock
  , parseXMLList
  ]

parseXMLInlineContent :: Parser Content
parseXMLInlineContent = choice
  [ parseXMLText
  , parseXMLBold
  , parseXMLItalic
  , parseXMLCode
  , parseXMLLink
  , parseXMLImage
  ]

parseXMLContent :: Parser Content
parseXMLContent = parseXMLBlockContent <|> parseXMLInlineContent

parseXMLText :: Parser Content
parseXMLText = do
  spaces
  text <- some (satisfy (\c -> c /= '<' && c /= '>'))
  spaces
  return (Text text)

parseXMLParagraph :: Parser Content
parseXMLParagraph = do
  (_, content) <- parseElement "paragraph" (many parseXMLContent)
  return (Paragraph content)

parseXMLSection :: Parser Content
parseXMLSection = do
  (attrs, content) <- parseElement "section" (many parseXMLContent)
  let title = fromMaybe "" (lookup "title" attrs)
  return (Section title content)

parseXMLBoldContent :: Parser Content
parseXMLBoldContent = choice
  [ parseXMLText
  , parseXMLItalic
  , parseXMLCode
  ]

parseXMLBold :: Parser Content
parseXMLBold = do
  (_, content) <- parseElement "bold" parseXMLBoldContent
  return (Bold content)

parseXMLItalicContent :: Parser Content
parseXMLItalicContent = choice
  [ parseXMLText
  , parseXMLBold
  , parseXMLCode
  ]

parseXMLItalic :: Parser Content
parseXMLItalic = do
  (_, content) <- parseElement "italic" parseXMLItalicContent
  return (Italic content)

parseXMLCode :: Parser Content
parseXMLCode = do
  (_, content) <- parseElement "code" (Text <$> many (satisfy (/= '<')))
  return $ case content of
    Text t -> Code t
    _ -> Code ""

parseXMLCodeBlock :: Parser Content
parseXMLCodeBlock = do
  (_, content) <- parseElement "codeblock" (Text <$> many (satisfy (/= '<')))
  return $ case content of
    Text t -> CodeBlock t
    _ -> CodeBlock ""

parseXMLLink :: Parser Content
parseXMLLink = do
  (attrs, _) <- parseElementOrSelfClosing "link" (return ())
  let text = fromMaybe "" (lookup "text" attrs)
  let url = fromMaybe "" (lookup "url" attrs)
  return (Link text url)

parseXMLImage :: Parser Content
parseXMLImage = do
  (attrs, _) <- parseElementOrSelfClosing "image" (return ())
  let alt = fromMaybe "" (lookup "alt" attrs)
  let url = fromMaybe "" (lookup "url" attrs)
  return (Image alt url)

parseXMLList :: Parser Content
parseXMLList = do
  (_, items) <- parseElement "list" (many parseXMLItem)
  return (List items)

parseXMLItem :: Parser Item
parseXMLItem = do
  (_, content) <- parseElement "item" (many parseXMLContent)
  return (Item content)

choice :: [Parser a] -> Parser a
choice [] = Parser $ \_ -> Nothing
choice (p:ps) = p <|> choice ps

parseXML :: String -> Maybe Document
parseXML input = case runParser parseXMLDocument input of
  Just (doc, rest) | all isSpace rest -> Just doc
  _ -> Nothing