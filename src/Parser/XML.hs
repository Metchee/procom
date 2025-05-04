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

parseAttribute :: Parser (String, String)
parseAttribute = (,)
  <$> some (satisfy (\c -> isAlphaNum c || c == '_' || c == '-'))
  <*  spaces
  <*  char '='
  <*  spaces
  <*  (char '"' <|> char '\'')
  <*> many (satisfy (\c -> c /= '"' && c /= '\''))
  <*  (char '"' <|> char '\'')
  <*  spaces

parseTagOpen :: String -> Parser ()
parseTagOpen tag = parseSimpleTag <|> parseWithAttributes
  where
    parseSimpleTag = parseTagPrefix >> string ">" >> tagSuffix
    
    parseWithAttributes = 
      parseTagPrefix >> 
      many parseAttribute >>= \_ ->
      spaces >>
      string ">" >>
      tagSuffix
    
    parseTagPrefix =
      spaces >>
      string "<" >>
      spaces >>
      string tag >>
      spaces
      
    tagSuffix = spaces >> return ()

parseTagClose :: String -> Parser ()
parseTagClose tag = 
  (spaces *> string ("</" ++ tag ++ ">") *> spaces *> return ())
  <|> (spaces *> string ("</" ++ tag) *> spaces *> string ">" *> spaces *> return ())

parseTagWithContent :: String -> Parser a -> Parser a
parseTagWithContent tag contentParser =
  parseTagOpen tag *> spaces *> contentParser <* spaces <* parseTagClose tag

parseTagWithAttrs :: String -> Parser a -> Parser ([(String, String)], a)
parseTagWithAttrs tag contentParser =
  parseAttrsPrefix tag >>= \attrs ->
  contentParser >>= \content ->
  parseAttrsSuffix tag >>
  return (attrs, content)

parseAttrsPrefix :: String -> Parser [(String, String)]
parseAttrsPrefix tag =
  string ("<" ++ tag) >>
  spaces >>
  many parseAttribute >>= \attrs ->
  spaces >>
  (string ">" <|> string "/>") >>
  spaces >>
  return attrs

parseAttrsSuffix :: String -> Parser ()
parseAttrsSuffix tag =
  spaces >>
  (parseTagClose tag <|> return ())

parseXMLDocument :: Parser Document
parseXMLDocument =
  spaces >>
  parseDocStart >>
  parseHeader >>= \header ->
  parseBody >>= \body ->
  parseDocEnd >>
  return (Document header body)

parseDocStart :: Parser ()
parseDocStart =
  (string "<document" <|> string "<?xml") >>
  return () >>
  (parseTagOpen "document" <|> parseDocStartAlt)
  where
    parseDocStartAlt =
      spaces >>
      many parseAttribute >>= \_ ->
      spaces >>
      string ">" >>
      return ()

parseDocEnd :: Parser ()
parseDocEnd =
  (parseTagClose "document" <|> return ()) >>
  spaces >>
  return ()

parseHeader :: Parser Header
parseHeader =
  (parseTagWithAttrs "header" (return ()) <|> parseEmptyHeader) >>= \pair ->
  let (attrs, _) = pair
  in return $ Header 
       (fromMaybe "" (lookup "title" attrs))
       (lookup "author" attrs)
       (lookup "date" attrs)
  where
    parseEmptyHeader = return ([], ())

parseBody :: Parser [Content]
parseBody =
  let parseBodyContents = parseTagWithContent "body" (many parseContent)
      parseEmptyBody = return []
  in parseBodyContents <|> parseEmptyBody

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
parseItem = Item <$> (parseTagWithContent "item" (many parseContent)
                 <|> return [Text ""])

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
parseLink = parseAttributedTag "link" Link <|> parseLinkWithContent

parseLinkWithContent :: Parser Content
parseLinkWithContent = do
  string "<link"
  spaces
  attrs <- many parseAttribute
  string ">"
  content <- many (satisfy (/= '<'))
  string "</link>"
  spaces
  let url = fromMaybe "" (lookup "url" attrs)
  return $ Link content url

parseImage :: Parser Content
parseImage = parseAttributedTag "image" Image <|> parseImageWithContent

parseImageWithContent :: Parser Content
parseImageWithContent = do
  string "<image"
  spaces
  attrs <- many parseAttribute
  string ">"
  content <- many (satisfy (/= '<'))
  string "</image>"
  spaces
  let url = fromMaybe "" (lookup "url" attrs)
  return $ Image content url

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

choice :: [Parser a] -> Parser a
choice [] = Parser $ \_ -> Nothing
choice (p:ps) = p <|> choice ps

parseXML :: String -> Maybe Document
parseXML input = 
  case runParser parseXMLDocument input of
    Just (doc, rest) | all isSpace rest -> Just doc
    _ -> 
      Just (Document (Header "" Nothing Nothing) [])