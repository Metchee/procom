{-
-- EPITECH PROJECT, 2025
-- procom
-- File description:
-- Markdown
-}

module Parser.Markdown where

import Parser.Core
import Document.Types
import Control.Applicative (many, some, (<|>), optional)
import Data.Char (isSpace, isAlpha, isAlphaNum)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe)

parseMarkdownDocument :: Parser Document
parseMarkdownDocument = do
  header <- parseMarkdownHeader <|> parseEmptyHeader
  body <- parseMarkdownBody
  return $ Document header body
  where
    parseEmptyHeader = return (Header "" Nothing Nothing)

parseMarkdownHeader :: Parser Header
parseMarkdownHeader = do
  fields <- parseHeaderFields
  return $ buildHeader fields

parseHeaderFields :: Parser [(String, String)]
parseHeaderFields = do
  parseHeaderStart
  fields <- many parseHeaderField
  parseHeaderEnd
  return fields

parseHeaderStart :: Parser ()
parseHeaderStart =
  string "---" >>
  spaces >>
  return ()

parseHeaderEnd :: Parser ()
parseHeaderEnd =
  spaces >>
  string "---" >>
  spaces >>
  return ()

buildHeader :: [(String, String)] -> Header
buildHeader fields = Header 
  (findField "title" fields)
  (findOptionalField "author" fields)
  (findOptionalField "date" fields)

parseHeaderField :: Parser (String, String)
parseHeaderField = do
  key <- parseFieldKey
  value <- parseFieldValue
  spaces
  return (key, value)

parseFieldKey :: Parser String
parseFieldKey = do
  spaces
  key <- some (satisfy (\c -> isAlphaNum c || c == '_'))
  spaces
  char ':'
  spaces
  return key

parseFieldValue :: Parser String
parseFieldValue = some (satisfy (/= '\n'))

parseMarkdownBody :: Parser [Content]
parseMarkdownBody = many parseMarkdownContent

parseMarkdownContent :: Parser Content
parseMarkdownContent = 
  parseList 
  <|> parseSection 
  <|> parseCodeBlock 
  <|> parseParagraph

parseParagraph :: Parser Content
parseParagraph = do
  contents <- some parseInlineContent
  many (char '\n')
  return $ Paragraph contents

parseSection :: Parser Content
parseSection = do
  level <- some (char '#')
  spaces
  title <- some (satisfy (/= '\n'))
  many (char '\n')
  contents <- many parseMarkdownContent
  return $ Section (Just title) contents

parseCodeBlock :: Parser Content
parseCodeBlock = do
  string "```"
  optional (some (satisfy (/= '\n')))
  many (char '\n')
  code <- tillString "```"
  string "```"
  many (char '\n')
  return $ CodeBlock [Text code]

parseList :: Parser Content
parseList = do
  items <- some parseListItem
  return $ List items

parseListItem :: Parser Item
parseListItem = do
  spaces
  char '-' <|> char '*' <|> char '+'
  spaces
  content <- many parseInlineContent
  many (char '\n')
  return $ Item content

parseInlineContent :: Parser Content
parseInlineContent = 
  parseBold 
  <|> parseItalic 
  <|> parseCode 
  <|> parseLink 
  <|> parseImage 
  <|> parseSimpleText

parseBold :: Parser Content
parseBold = do
  string "**"
  content <- tillString "**"
  string "**"
  return $ Bold (Text content)

parseItalic :: Parser Content
parseItalic = do
  char '*'
  content <- tillString "*"
  char '*'
  return $ Italic (Text content)

parseCode :: Parser Content
parseCode = do
  char '`'
  code <- tillString "`"
  char '`'
  return $ Code code

parseLink :: Parser Content
parseLink = do
  char '['
  text <- tillString "]"
  string "]("
  url <- tillString ")"
  char ')'
  return $ Link text url

parseImage :: Parser Content
parseImage = do
  string "!["
  alt <- parseAltText
  string "]("
  url <- parseImageUrl
  char ')'
  return $ Image alt url
  where
    parseAltText = tillString "]"
    parseImageUrl = tillString ")"

parseSimpleText :: Parser Content
parseSimpleText = 
  Text <$> some (satisfy notSpecialChar)
  where
    notSpecialChar c = c /= '*' && c /= '`' && 
                       c /= '[' && c /= '!' && c /= '\n'

parseMarkdown :: String -> Maybe Document
parseMarkdown input = 
  case runParser parseMarkdownDocument input of
    Just (doc, rest) | all isSpace rest -> Just doc
    _ -> Just (Document (Header "" Nothing Nothing) [])

findField :: String -> [(String, String)] -> String
findField key fields = 
  case lookup key fields of
    Just value -> value
    Nothing -> ""

findOptionalField :: String -> [(String, String)] -> Maybe String
findOptionalField key fields =
  lookup key fields