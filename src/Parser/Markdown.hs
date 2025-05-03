{-
-- EPITECH PROJECT, 2025
-- procom
-- File description:
-- Markdown
-}

module Parser.Markdown where

import Parser.Core
import Document.Types
import Control.Applicative (many, some, (<|>))
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Char (isSpace, isAlpha)

parseMarkdownDocument :: Parser Document
parseMarkdownDocument = do
  header <- parseMarkdownHeader
  body <- parseMarkdownBody
  return $ Document header body

parseMarkdownHeader :: Parser Header
parseMarkdownHeader = do
  fields <- parseHeaderFields
  let title = findField "title" fields
  let author = findField "author" fields
  let date = findField "date" fields
  return $ Header title (Just author) (Just date)

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
  key <- some (satisfy (\c -> isAlpha c || c == '_'))
  spaces
  char ':'
  spaces
  return key

parseFieldValue :: Parser String
parseFieldValue = do
  value <- some (satisfy (/= '\n'))
  spaces
  return value

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
  title <- some (satisfy (/= '\n'))
  spaces
  content <- many parseMarkdownContent
  return $ Section title content

parseCodeBlock :: Parser Content
parseCodeBlock = do
  string "```"
  spaces
  code <- tillString "```"
  string "```"
  spaces
  return $ CodeBlock code

parseList :: Parser Content
parseList = do
  items <- some parseListItem
  return $ List items
  where
    parseListItem = do
      char '-'
      spaces
      content <- many parseInlineContent
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
  content <- parseSimpleText
  char '*'
  return $ Italic content

parseBold :: Parser Content
parseBold = do
  string "**"
  content <- parseSimpleText
  string "**"
  return $ Bold content

parseCode :: Parser Content
parseCode = do
  char '`'
  code <- some (satisfy (/= '`'))
  char '`'
  return $ Code code

parseLink :: Parser Content
parseLink = do
  char '['
  text <- some (satisfy (/= ']'))
  string "]("
  url <- some (satisfy (/= ')'))
  char ')'
  return $ Link text url

parseImage :: Parser Content
parseImage = do
  string "!["
  alt <- some (satisfy (/= ']'))
  string "]("
  url <- some (satisfy (/= ')'))
  char ')'
  return $ Image alt url

parseSimpleText :: Parser Content
parseSimpleText = Text <$>
  some(satisfy (\c -> c /= '*' && c /= '`' && c /= '[' && c /= '\n'))

parseMarkdown :: String -> Maybe Document
parseMarkdown input = case runParser parseMarkdownDocument input of
  Just (doc, "") -> Just doc
  Just (doc, rest) | all isSpace rest -> Just doc
  _ -> Nothing

findField :: String -> [(String, String)] -> String
findField key fields = case lookup key fields of
  Just value -> value
  Nothing -> ""
