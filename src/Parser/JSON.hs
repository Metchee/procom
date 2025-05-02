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
import Data.Maybe (catMaybes, fromMaybe)

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
  headerObj <- (lookup "header" obj >>= getObject) <|> Just []
  let title = (lookup "title" headerObj >>= getString) <|> Just ""
  let author = lookup "author" headerObj >>= getString
  let date = lookup "date" headerObj >>= getString
  return $ Header (fromMaybe "" title) author date

getBody :: [(String, JSONValue)] -> Maybe [Content]
getBody obj = do
  bodyArr <- (lookup "body" obj >>= getArray) <|> Just []
  result <- (mapM jsonValueToContent bodyArr) <|> Just []
  return $ if null result then [Text ""] else result

jsonToDocument :: JSONValue -> Maybe Document
jsonToDocument (JSONObject obj) = do
  header <- getHeader obj <|> Just (Header "" Nothing Nothing)
  body <- getBody obj <|> Just []
  return $ Document header body
jsonToDocument _ = Just (Document (Header "" Nothing Nothing) [])

jsonValueToItem :: JSONValue -> Maybe Item
jsonValueToItem (JSONObject obj) = do
  content <- (lookup "content" obj >>= getArray) <|> Just []
  contentList <- (mapM jsonValueToContent content) <|> Just []
  return $ Item contentList
jsonValueToItem (JSONArray arr) = do
  contentList <- (mapM jsonValueToContent arr) <|> Just []
  return $ Item contentList
jsonValueToItem (JSONString s) = Just (Item [Text s])
jsonValueToItem _ = Just (Item [])

parseParagraphContent :: [(String, JSONValue)] -> Maybe Content
parseParagraphContent obj = do
  content <- (lookup "content" obj >>= getArray) <|> Just []
  contentList <- (mapM jsonValueToContent content) <|> Just [Text ""]
  return $ Paragraph contentList

parseSectionContent :: [(String, JSONValue)] -> Maybe Content
parseSectionContent obj = do
  title <- (lookup "title" obj >>= getString) <|> Just ""
  content <- (lookup "content" obj >>= getArray) <|> Just []
  contentList <- (mapM jsonValueToContent content) <|> Just []
  return $ Section title contentList

parseItalicContent :: [(String, JSONValue)] -> Maybe Content
parseItalicContent obj = do
  content <- (lookup "content" obj >>= jsonValueToContent) <|> Just (Text "")
  return $ Italic content

parseBoldContent :: [(String, JSONValue)] -> Maybe Content
parseBoldContent obj = do
  content <- (lookup "content" obj >>= jsonValueToContent) <|> Just (Text "")
  return $ Bold content

parseCodeContent :: [(String, JSONValue)] -> Maybe Content
parseCodeContent obj = do
  code <- (lookup "content" obj >>= getString) <|> Just ""
  return $ Code code

parseLinkContent :: [(String, JSONValue)] -> Maybe Content
parseLinkContent obj = do
  text <- ((lookup "text" obj >>= getString) <|> 
          (lookup "content" obj >>= getString)) <|> Just "link"
  url <- (lookup "url" obj >>= getString) <|> Just "#"
  return $ Link text url

parseImageContent :: [(String, JSONValue)] -> Maybe Content
parseImageContent obj = do
  alt <- (lookup "alt" obj >>= getString) <|> Just "image"
  url <- (lookup "url" obj >>= getString) <|> Just "#"
  return $ Image alt url

parseCodeBlockContent :: [(String, JSONValue)] -> Maybe Content
parseCodeBlockContent obj = do
  code <- (lookup "content" obj >>= getString) <|> Just ""
  return $ CodeBlock code

parseListContent :: [(String, JSONValue)] -> Maybe Content
parseListContent obj = do
  items <- (lookup "items" obj >>= getArray) <|> Just []
  itemsList <- (mapM jsonValueToItem items) <|> Just []
  return $ List (if null itemsList then [Item [Text ""]] else itemsList)

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
selectContentParser _ = \obj -> Just (Text (show obj))

jsonValueToContent :: JSONValue -> Maybe Content
jsonValueToContent (JSONString s) = Just (Text s)
jsonValueToContent (JSONObject obj) = do
  typeStr <- (lookup "type" obj >>= getString) <|> Just "text"
  result <- selectContentParser typeStr obj
  return result
jsonValueToContent (JSONArray arr) =
  Just (Paragraph (catMaybes (map jsonValueToContent arr)))
jsonValueToContent (JSONNumber n) = Just (Text (show n))
jsonValueToContent (JSONBool b) = Just (Text (show b))
jsonValueToContent JSONNull = Just (Text "")

parseJSON :: String -> Maybe Document
parseJSON input = case runParser parseJSONValue input of
  Just (value, rest) | all isSpace rest -> jsonToDocument value
  _ -> 
    Just (Document (Header "" Nothing Nothing) [Text "Parse error"])