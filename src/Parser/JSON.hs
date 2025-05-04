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
import Data.List (isPrefixOf)

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

getNumber :: JSONValue -> Maybe Int
getNumber (JSONNumber n) = Just n
getNumber _ = Nothing

getBool :: JSONValue -> Maybe Bool
getBool (JSONBool b) = Just b
getBool _ = Nothing

getHeader :: [(String, JSONValue)] -> Maybe Header
getHeader obj = do
  headerObj <- lookup "header" obj >>= getObject
  let title = lookup "title" headerObj >>= getString
  let author = lookup "author" headerObj >>= getString
  let date = lookup "date" headerObj >>= getString
  return $ Header (fromMaybe "" title) author date

getBody :: [(String, JSONValue)] -> Maybe [Content]
getBody obj = do
  bodyArr <- lookup "body" obj >>= getArray
  mapM jsonValueToContent bodyArr

parseJSON :: String -> Maybe Document
parseJSON input = 
  case runParser parseJSONValue input of
    Just (value, rest) | all isSpace rest -> jsonToDocument value
    _ -> Nothing

jsonToDocument :: JSONValue -> Maybe Document
jsonToDocument (JSONObject obj) = do
  header <- getHeader obj
  body <- getBody obj
  return $ Document header body
jsonToDocument _ = Nothing

jsonValueToContent :: JSONValue -> Maybe Content
jsonValueToContent (JSONString s) = Just (Text s)
jsonValueToContent (JSONObject obj) = 
  parseStandardObject obj <|> 
  parseDirectKeyObject obj <|> 
  Just (Text (show obj))
jsonValueToContent (JSONArray arr) = 
  let contents = catMaybes (map jsonValueToContent arr)
  in Just (Paragraph contents)
jsonValueToContent _ = Nothing

parseDirectKeyObject :: [(String, JSONValue)] -> Maybe Content
parseDirectKeyObject obj
  | length obj == 1 = 
      case head obj of
        ("paragraph", JSONString content) -> 
          Just (Paragraph [Text content])
        ("section", JSONObject sectionObj) -> 
          parseSectionFromObj sectionObj
        ("codeblock", JSONString content) -> 
          Just (CodeBlock content)
        ("list", JSONArray items) -> 
          parseListItems items
        ("link", JSONObject linkObj) -> 
          parseLinkFromObj linkObj
        ("image", JSONObject imgObj) -> 
          parseImageFromObj imgObj
        ("italic", JSONString content) -> 
          Just (Italic (Text content))
        ("bold", JSONString content) -> 
          Just (Bold (Text content))
        ("code", JSONString content) -> 
          Just (Code content)
        _ -> Nothing
  | otherwise = Nothing

parseSectionFromObj :: [(String, JSONValue)] -> Maybe Content
parseSectionFromObj obj = do
  title <- lookup "title" obj >>= getString
  content <- lookup "content" obj >>= getArray
  contents <- mapM jsonValueToContent content
  return (Section title contents)

parseLinkFromObj :: [(String, JSONValue)] -> Maybe Content
parseLinkFromObj obj = do
  text <- lookup "text" obj >>= getString
  url <- lookup "url" obj >>= getString
  return (Link text url)

parseImageFromObj :: [(String, JSONValue)] -> Maybe Content
parseImageFromObj obj = do
  alt <- lookup "alt" obj >>= getString
  url <- lookup "url" obj >>= getString
  return (Image alt url)

parseListItems :: [JSONValue] -> Maybe Content
parseListItems items = do
  itemsList <- mapM parseListItem items
  return (List itemsList)

parseListItem :: JSONValue -> Maybe Item
parseListItem (JSONObject obj) = do
  contentArr <- lookup "content" obj >>= getArray
  contents <- mapM jsonValueToContent contentArr
  return (Item contents)
parseListItem _ = Nothing

parseStandardObject :: [(String, JSONValue)] -> Maybe Content
parseStandardObject obj = do
  typeVal <- lookup "type" obj >>= getString
  case typeVal of
    "paragraph" -> parseParagraph obj
    "codeblock" -> parseCodeBlock obj
    "section" -> parseSection obj
    "list" -> parseList obj
    "link" -> parseLink obj
    "image" -> parseImage obj
    "italic" -> parseItalicContent obj
    "bold" -> parseBoldContent obj
    "code" -> parseCodeContent obj
    _ -> Nothing

parseParagraph :: [(String, JSONValue)] -> Maybe Content
parseParagraph obj = do
  contentArr <- lookup "content" obj >>= getArray
  contents <- mapM jsonValueToContent contentArr
  return (Paragraph contents)

parseCodeBlock :: [(String, JSONValue)] -> Maybe Content
parseCodeBlock obj = do
  content <- lookup "content" obj >>= getString
  return (CodeBlock content)

parseSection :: [(String, JSONValue)] -> Maybe Content
parseSection obj = do
  title <- lookup "title" obj >>= getString
  contentArr <- lookup "content" obj >>= getArray
  contents <- mapM jsonValueToContent contentArr
  return (Section title contents)

parseList :: [(String, JSONValue)] -> Maybe Content
parseList obj = do
  items <- lookup "items" obj >>= getArray
  itemsList <- mapM parseListItem items
  return (List itemsList)

parseLink :: [(String, JSONValue)] -> Maybe Content
parseLink obj = do
  text <- lookup "text" obj >>= getString
  url <- lookup "url" obj >>= getString
  return (Link text url)

parseImage :: [(String, JSONValue)] -> Maybe Content
parseImage obj = do
  alt <- lookup "alt" obj >>= getString
  url <- lookup "url" obj >>= getString
  return (Image alt url)

parseItalicContent :: [(String, JSONValue)] -> Maybe Content
parseItalicContent obj = do
  content <- lookup "content" obj >>= jsonValueToContent
  return (Italic content)

parseBoldContent :: [(String, JSONValue)] -> Maybe Content
parseBoldContent obj = do
  content <- lookup "content" obj >>= jsonValueToContent
  return (Bold content)

parseCodeContent :: [(String, JSONValue)] -> Maybe Content
parseCodeContent obj = do
  content <- lookup "content" obj >>= getString
  return (Code content)

guard :: Bool -> Maybe ()
guard True = Just ()
guard False = Nothing