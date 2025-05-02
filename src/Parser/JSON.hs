{-
-- EPITECH PROJECT, 2025
-- procom
-- File description:
-- JSON
-}

module Parser.JSON where

import Parser.Core
import Document.Types
import Control.Applicative (many, some, (<|>))
import Data.Char (isSpace)
import Data.Maybe (catMaybes)

data JSONValue
  = JSONString String
  | JSONNumber Int
  | JSONObject [(String, JSONValue)]
  | JSONArray [JSONValue]
  | JSONBool Bool
  | JSONNull
  deriving (Show, Eq)

parseJSONValue :: Parser JSONValue
parseJSONValue = parseJSONString 
             <|> parseJSONNumber 
             <|> parseJSONObject 
             <|> parseJSONArray 
             <|> parseJSONBool 
             <|> parseJSONNull

parseJSONString :: Parser JSONValue
parseJSONString = JSONString <$> quotedString

parseJSONNumber :: Parser JSONValue
parseJSONNumber = JSONNumber <$> int

parseJSONBool :: Parser JSONValue
parseJSONBool = (string "true" >> return (JSONBool True))
            <|> (string "false" >> return (JSONBool False))

parseJSONNull :: Parser JSONValue
parseJSONNull = string "null" >> return JSONNull

parseJSONArray :: Parser JSONValue
parseJSONArray = do
  char '['
  spaces
  values <- parseJSONValue `sepBy` (char ',' >> spaces)
  spaces
  char ']'
  return $ JSONArray values

parseJSONObject :: Parser JSONValue
parseJSONObject = 
  JSONObject <$> (char '{' *> spaces *> pairs <* spaces <* char '}')
  where
    pairs = parsePair `sepBy` (char ',' >> spaces)
    parsePair = (,)
      <$> (quotedString <* spaces <* char ':' <* spaces) <*> parseJSONValue

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = ((:) <$> p <*> many (sep >> p)) <|> pure []

getObject :: JSONValue -> Maybe [(String, JSONValue)]
getObject (JSONObject o) = Just o
getObject _ = Nothing

getArray :: JSONValue -> Maybe [JSONValue]
getArray (JSONArray arr) = Just arr
getArray _ = Nothing

getString :: JSONValue -> Maybe String
getString (JSONString s) = Just s
getString _ = Nothing

getHeader :: [(String, JSONValue)] -> Maybe Header
getHeader obj = do
  headerObj <- lookup "header" obj >>= getObject
  title <- lookup "title" headerObj >>= getString
  let author = lookup "author" headerObj >>= getString
  let date = lookup "date" headerObj >>= getString
  return $ Header title author date

getBody :: [(String, JSONValue)] -> Maybe [Content]
getBody obj = do
  bodyArr <- lookup "body" obj >>= getArray
  mapM jsonValueToContent bodyArr

jsonToDocument :: JSONValue -> Maybe Document
jsonToDocument (JSONObject obj) = do
  header <- getHeader obj
  body <- getBody obj
  return $ Document header body
jsonToDocument _ = Nothing

jsonValueToItem :: JSONValue -> Maybe Item
jsonValueToItem (JSONObject obj) = do
  content <- lookup "content" obj >>= getArray
  contentList <- mapM jsonValueToContent content
  return $ Item contentList
jsonValueToItem _ = Nothing

parseParagraphContent :: [(String, JSONValue)] -> Maybe Content
parseParagraphContent obj = do
  content <- lookup "content" obj >>= getArray
  contentList <- mapM jsonValueToContent content
  return $ Paragraph contentList

parseSectionContent :: [(String, JSONValue)] -> Maybe Content
parseSectionContent obj = do
  title <- lookup "title" obj >>= getString
  content <- lookup "content" obj >>= getArray
  contentList <- mapM jsonValueToContent content
  return $ Section title contentList

parseItalicContent :: [(String, JSONValue)] -> Maybe Content
parseItalicContent obj = do
  content <- lookup "content" obj >>= jsonValueToContent
  return $ Italic content

parseBoldContent :: [(String, JSONValue)] -> Maybe Content
parseBoldContent obj = do
  content <- lookup "content" obj >>= jsonValueToContent
  return $ Bold content

parseCodeContent :: [(String, JSONValue)] -> Maybe Content
parseCodeContent obj = do
  code <- lookup "content" obj >>= getString
  return $ Code code

parseLinkContent :: [(String, JSONValue)] -> Maybe Content
parseLinkContent obj = do
  text <- lookup "text" obj >>= getString
  url <- lookup "url" obj >>= getString
  return $ Link text url

parseImageContent :: [(String, JSONValue)] -> Maybe Content
parseImageContent obj = do
  alt <- lookup "alt" obj >>= getString
  url <- lookup "url" obj >>= getString
  return $ Image alt url

parseCodeBlockContent :: [(String, JSONValue)] -> Maybe Content
parseCodeBlockContent obj = do
  code <- lookup "content" obj >>= getString
  return $ CodeBlock code

parseListContent :: [(String, JSONValue)] -> Maybe Content
parseListContent obj = do
  items <- lookup "items" obj >>= getArray
  itemsList <- mapM jsonValueToItem items
  return $ List itemsList

selectContentParser :: String -> [(String, JSONValue)] -> Maybe Content
selectContentParser "paragraph" = parseParagraphContent
selectContentParser "section" = parseSectionContent
selectContentParser "italic" = parseItalicContent
selectContentParser "bold" = parseBoldContent
selectContentParser "code" = parseCodeContent
selectContentParser "link" = parseLinkContent
selectContentParser "image" = parseImageContent
selectContentParser "codeblock" = parseCodeBlockContent
selectContentParser "list" = parseListContent
selectContentParser _ = const Nothing

jsonValueToContent :: JSONValue -> Maybe Content
jsonValueToContent (JSONString s) = Just (Text s)
jsonValueToContent (JSONObject obj) = do
  typeStr <- lookup "type" obj >>= getString
  selectContentParser typeStr obj
jsonValueToContent _ = Nothing

parseJSON :: String -> Maybe Document
parseJSON input = case runParser parseJSONValue input of
  Just (value, "") -> jsonToDocument value
  Just (value, rest) | all isSpace rest -> jsonToDocument value
  _ -> Nothing
