module Parser.Markdown where

import Parser.Core
import Document.Types
import Control.Applicative (many, some, (<|>))
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Char (isSpace, isAlpha)
-- Parser pour un document Markdown
parseMarkdownDocument :: Parser Document
parseMarkdownDocument = do
  header <- parseMarkdownHeader
  body <- parseMarkdownBody
  return $ Document header body

-- Parser pour l'en-tête Markdown (YAML frontmatter)
parseMarkdownHeader :: Parser Header
parseMarkdownHeader = do
  string "---"
  spaces
  fields <- many parseHeaderField
  spaces
  string "---"
  spaces
  let title = findField "title" fields
  let author = findField "author" fields
  let date = findField "date" fields
  return $ Header title (Just author) (Just date)
  where
    parseHeaderField = do
      key <- some (satisfy (\c -> isAlpha c || c == '_'))
      spaces
      char ':'
      spaces
      value <- some (satisfy (/= '\n'))
      spaces
      return (key, value)
    
    findField key fields = case lookup key fields of
      Just value -> value
      Nothing -> ""

-- Parser pour le corps Markdown
parseMarkdownBody :: Parser [Content]
parseMarkdownBody = many parseMarkdownContent

-- Parser pour les différents types de contenu Markdown
parseMarkdownContent :: Parser Content
parseMarkdownContent = parseParagraph 
                    <|> parseSection 
                    <|> parseCodeBlock 
                    <|> parseList
                    <|> parseText

-- Parser pour un paragraphe
parseParagraph :: Parser Content
parseParagraph = do
  content <- some parseInlineContent
  spaces
  return $ Paragraph content

-- Parser pour une section (titre)
parseSection :: Parser Content
parseSection = do
  level <- some (char '#')
  spaces
  title <- some (satisfy (/= '\n'))
  spaces
  content <- many parseMarkdownContent
  return $ Section title content

-- Parser pour un bloc de code
parseCodeBlock :: Parser Content
parseCodeBlock = do
  string "```"
  spaces
  code <- tillString "```"
  string "```"
  spaces
  return $ CodeBlock code

-- Parser pour une liste
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

-- Parser pour du texte simple et formaté
parseText :: Parser Content
parseText = do
  text <- some (satisfy (/= '\n'))
  spaces
  return $ Text text

-- Parser pour le contenu en ligne (formatage, liens, etc.)
parseInlineContent :: Parser Content
parseInlineContent = parseItalic <|> parseBold <|> parseCode <|> parseLink <|> parseImage <|> parseSimpleText

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
parseSimpleText = Text <$> some (satisfy (\c -> c /= '*' && c /= '`' && c /= '[' && c /= '\n'))

-- Parse un document Markdown à partir d'une chaîne
parseMarkdown :: String -> Maybe Document
parseMarkdown input = case runParser parseMarkdownDocument input of
  Just (doc, "") -> Just doc
  Just (doc, rest) | all isSpace rest -> Just doc
  _ -> Nothing