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
import Data.Char (isSpace, isAlpha)
import Data.Maybe (catMaybes, fromMaybe)

parseAttribute :: Parser (String, String)
parseAttribute = (,)
  <$> some (satisfy (\c -> isAlpha c || c == '_'))
  <*  spaces
  <*  char '='
  <*  spaces
  <*  char '"'
  <*> many (satisfy (/= '"'))
  <*  char '"'
  <*  spaces

parseTagOpen :: String -> Parser ()
parseTagOpen tag = 
  string ("<" ++ tag) *> spaces *> string ">" *> spaces *> return ()

parseTagClose :: String -> Parser ()
parseTagClose tag = 
  string ("</" ++ tag ++ ">") *> spaces *> return ()

parseTagWithContent :: String -> Parser a -> Parser a
parseTagWithContent tag contentParser =
  parseTagOpen tag *> contentParser <* parseTagClose tag

parseTagWithAttrs :: String -> Parser a -> Parser ([(String, String)], a)
parseTagWithAttrs tag contentParser = 
  (,) <$> (string ("<" ++ tag) *> spaces *> many parseAttribute
      <* spaces <* string ">" <* spaces)
      <*> (contentParser <* spaces <* parseTagClose tag)

parseXMLDocument :: Parser Document
parseXMLDocument = 
  spaces *>
  parseTagOpen "document" *>
  (Document <$> parseHeader <*> parseBody) <*
  parseTagClose "document" <*
  spaces

parseHeader :: Parser Header
parseHeader = do
  (attrs, _) <- parseTagWithAttrs "header" (return ())
  return $ Header 
    (fromMaybe "" (lookup "title" attrs))
    (lookup "author" attrs)
    (lookup "date" attrs)

parseBody :: Parser [Content]
parseBody = parseTagWithContent "body" (many parseContent)

parseContent :: Parser Content
parseContent = parseParagraph 
           <|> parseSection 
           <|> parseList 
           <|> parseFormatting
           <|> parseSpecialContent
           <|> parseText
  where
    parseFormatting = parseBold <|> parseItalic <|> parseCode
    parseSpecialContent = parseCodeBlock <|> parseLink <|> parseImage

choice :: [Parser a] -> Parser a
choice [] = Parser $ \_ -> Nothing
choice (p:ps) = p <|> choice ps

parseParagraph :: Parser Content
parseParagraph = Paragraph
  <$> parseTagWithContent "paragraph" (many parseContent)

parseSection :: Parser Content
parseSection = do
  (attrs, contents) <- parseTagWithAttrs "section" (many parseContent)
  return $ Section (fromMaybe "" (lookup "title" attrs)) contents

parseList :: Parser Content
parseList = List <$> parseTagWithContent "list" (many parseItem)

parseItem :: Parser Item
parseItem = Item <$> parseTagWithContent "item" (many parseContent)

parseText :: Parser Content
parseText = Text <$> some (satisfy (\c -> c /= '<' && c /= '>'))

parseBold :: Parser Content
parseBold = Bold <$> parseTagWithContent "bold" parseContentInline

parseItalic :: Parser Content
parseItalic = Italic <$> parseTagWithContent "italic" parseContentInline

parseCode :: Parser Content
parseCode = Code <$> parseCodeContent
  where parseCodeContent = parseTagOpen "code" *> 
                           many (satisfy (/= '<')) <* 
                           parseTagClose "code"

parseCodeBlock :: Parser Content
parseCodeBlock = CodeBlock <$> parseCodeBlockContent
  where 
    parseCodeBlockContent = 
      parseTagOpen "codeblock" *>
      many (satisfy (/= '<')) <*
      parseTagClose "codeblock"

parseLink :: Parser Content
parseLink = parseAttributedTag "link" Link

parseImage :: Parser Content
parseImage = parseAttributedTag "image" Image

parseAttributedTag :: String -> (String -> String -> Content) -> Parser Content
parseAttributedTag tag constructor = do
  (attrs, _) <- parseTagWithAttrs tag (return ())
  let attr1 = case tag of
              "link" -> fromMaybe "" (lookup "text" attrs)
              "image" -> fromMaybe "" (lookup "alt" attrs)
              _ -> ""
  let attr2 = fromMaybe "" (lookup "url" attrs)
  return $ constructor attr1 attr2

parseContentInline :: Parser Content
parseContentInline = choice
  [ parseText
  , parseInlineBold
  , parseInlineItalic
  , parseInlineCode
  ]

parseInlineBold :: Parser Content
parseInlineBold = Bold <$> 
  (string "<bold>" *> parseContentInline <* string "</bold>")

parseInlineItalic :: Parser Content
parseInlineItalic = Italic <$>
  (string "<italic>" *> parseContentInline <* string "</italic>")

parseInlineCode :: Parser Content
parseInlineCode = Code <$>
  (string "<code>" *> many (satisfy (/= '<')) <* string "</code>")

parseXML :: String -> Maybe Document
parseXML input = case runParser parseXMLDocument input of
  Just (doc, rest) | all isSpace rest -> Just doc
  _ -> Nothing
