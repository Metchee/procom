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
  body <- parseMarkdownBody <|> return [Text ""]
  return $ Document header body
  where
    parseEmptyHeader = return (Header "" Nothing Nothing)

parseMarkdownHeader :: Parser Header
parseMarkdownHeader = do
  fields <- parseHeaderFields <|> return []
  let title = findField "title" fields
  let author = findOptionalField "author" fields
  let date = findOptionalField "date" fields
  return $ Header title author date

parseHeaderFields :: Parser [(String, String)]
parseHeaderFields = do
  string "---"
  spaces
  fields <- many parseHeaderField
  spaces
  string "---"
  spaces
  return fields

parseHeaderField :: Parser (String, String)
parseHeaderField = do
  key <- parseFieldKey
  value <- parseFieldValue
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
parseFieldValue = do
  value <- some (satisfy (/= '\n')) <|> return ""
  spaces
  return (dropWhile isSpace value)

parseMarkdownBody :: Parser [Content]
parseMarkdownBody = many parseMarkdownContent

parseMarkdownContent :: Parser Content
parseMarkdownContent = parseParagraph 
                    <|> parseSection 
                    <|> parseCodeBlock 
                    <|> parseList
                    <|> parseText

parseParagraph :: Parser Content
parseParagraph = do
  content <- some parseInlineContent
  spaces
  return $ Paragraph content

parseSection :: Parser Content
parseSection = do
  level <- some (char '#')
  spaces
  title <- some (satisfy (/= '\n')) <|> return ""
  spaces
  content <- many parseMarkdownContent <|> return []
  return $ Section (dropWhile isSpace title) content

parseCodeBlock :: Parser Content
parseCodeBlock = do
  string "```"
  spaces
  code <- tillString "```" <|> return ""
  string "```" <|> return ""
  spaces
  return $ CodeBlock code

parseList :: Parser Content
parseList = do
  items <- some parseListItem
  return $ List items
  where
    parseListItem = do
      spaces
      char '-' <|> char '*' <|> char '+'
      spaces
      content <- many parseInlineContent <|> return [Text ""]
      spaces
      return $ Item content

parseText :: Parser Content
parseText = do
  text <- some (satisfy (/= '\n'))
  spaces
  return $ Text text

parseInlineContent :: Parser Content
parseInlineContent = parseItalic <|> parseBold <|>
  parseCode <|> parseLink <|> parseImage <|> parseSimpleText

parseItalic :: Parser Content
parseItalic = do
  char '*'
  content <- parseSimpleText <|> return (Text "")
  char '*'
  return $ Italic content

parseBold :: Parser Content
parseBold = do
  string "**"
  content <- parseSimpleText <|> return (Text "")
  string "**"
  return $ Bold content

parseCode :: Parser Content
parseCode = do
  char '`'
  code <- many (satisfy (/= '`')) <|> return ""
  char '`'
  return $ Code code

parseLink :: Parser Content
parseLink = do
  char '['
  text <- many (satisfy (/= ']')) <|> return ""
  string "]("
  url <- many (satisfy (/= ')')) <|> return ""
  char ')'
  return $ Link text url

parseImage :: Parser Content
parseImage = do
  string "!["
  alt <- many (satisfy (/= ']')) <|> return ""
  string "]("
  url <- many (satisfy (/= ')')) <|> return ""
  char ')'
  return $ Image alt url

parseSimpleText :: Parser Content
parseSimpleText = Text <$>
  some (satisfy (\c -> c /= '*' && c /= '`' && c /= '[' && c /= '\n'))
  <|> return (Text "")

parseMarkdown :: String -> Maybe Document
parseMarkdown input = 
  case runParser parseMarkdownDocument input of
    Just (doc, rest) | all isSpace rest -> Just doc
    _ -> Just (Document (Header "" Nothing Nothing) [Text ""])

findField :: String -> [(String, String)] -> String
findField key fields = 
  case lookup key fields of
    Just value -> value
    Nothing -> ""

findOptionalField :: String -> [(String, String)] -> Maybe String
findOptionalField key fields =
  lookup key fields
