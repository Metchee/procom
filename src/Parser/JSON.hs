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
jsonValueToContent (JSONArray arr) = do
  contents <- mapM jsonValueToContent arr
  return (Paragraph contents)
jsonValueToContent (JSONObject obj) = 
  parseComplexObject obj <|>
  parseStandardObject obj <|> 
  Just (Text (show obj))
jsonValueToContent _ = Nothing

parseComplexObject :: [(String, JSONValue)] -> Maybe Content
parseComplexObject obj
  | length obj == 1 = case head obj of
      ("section", sectionValue) -> parseSectionValue sectionValue
      ("list", listValue) -> parseListValue listValue
      ("link", linkValue) -> parseLinkValue linkValue
      ("image", imageValue) -> parseImageValue imageValue
      ("codeblock", codeblockValue) -> parseCodeblockValue codeblockValue
      ("bold", boldValue) -> parseBoldValue boldValue
      ("italic", italicValue) -> parseItalicValue italicValue
      ("code", codeValue) -> parseCodeValue codeValue
      _ -> Nothing
  | otherwise = Nothing

parseSectionValue :: JSONValue -> Maybe Content
parseSectionValue (JSONObject obj) = do
  title <- lookup "title" obj >>= getString
  content <- lookup "content" obj >>= getArray
  contents <- mapM jsonValueToContent content
  return (Section (Just title) contents)
parseSectionValue _ = Nothing

-- Improved list parsing to handle both formats
parseListValue :: JSONValue -> Maybe Content
parseListValue (JSONArray items) = do
  listItems <- mapM parseListItem items
  return (List listItems)
parseListValue _ = Nothing

parseListItem :: JSONValue -> Maybe Item
parseListItem (JSONObject obj) = do
  contentArr <- lookup "content" obj >>= getArray
  contents <- mapM jsonValueToContent contentArr
  return (Item contents)
parseListItem (JSONArray content) = do
  contents <- mapM jsonValueToContent content
  return (Item contents)
parseListItem (JSONString s) = Just (Item [Text s])
parseListItem _ = Nothing

parseLinkValue :: JSONValue -> Maybe Content
parseLinkValue (JSONObject obj) = do
  url <- lookup "url" obj >>= getString
  
  -- Try different approaches to get link text
  textMaybe <- case lookup "text" obj of
                 Just (JSONString s) -> Just s
                 _ -> case lookup "content" obj of
                        Just (JSONArray [JSONString s]) -> Just s
                        _ -> Just "link"
  
  return (Link textMaybe url)
parseLinkValue _ = Nothing

parseImageValue :: JSONValue -> Maybe Content
parseImageValue (JSONObject obj) = do
  url <- lookup "url" obj >>= getString
  
  -- Try different approaches to get alt text
  altMaybe <- case lookup "alt" obj of
                Just (JSONString s) -> Just s
                _ -> case lookup "alt" obj of
                       Just (JSONArray [JSONString s]) -> Just s
                       _ -> Just "image"
  
  return (Image altMaybe url)
parseImageValue _ = Nothing

parseCodeblockValue :: JSONValue -> Maybe Content
parseCodeblockValue (JSONString code) = 
  Just (CodeBlock [Text code])
parseCodeblockValue (JSONArray content) = do
  contents <- mapM jsonValueToContent content
  return (CodeBlock contents)
parseCodeblockValue _ = Nothing

parseBoldValue :: JSONValue -> Maybe Content
parseBoldValue (JSONString text) = Just (Bold (Text text))
parseBoldValue (JSONObject obj) = do
  content <- lookup "content" obj >>= jsonValueToContent
  return (Bold content)
parseBoldValue _ = Nothing

parseItalicValue :: JSONValue -> Maybe Content
parseItalicValue (JSONString text) = Just (Italic (Text text))
parseItalicValue (JSONObject obj) = do
  content <- lookup "content" obj >>= jsonValueToContent
  return (Italic content)
parseItalicValue _ = Nothing

parseCodeValue :: JSONValue -> Maybe Content
parseCodeValue (JSONString text) = Just (Code text)
parseCodeValue (JSONObject obj) = do
  content <- lookup "content" obj >>= getString
  return (Code content)
parseCodeValue _ = Nothing

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
  contentArr <- lookup "content" obj >>= getArray
  contents <- mapM jsonValueToContent contentArr
  return (CodeBlock contents)

parseSection :: [(String, JSONValue)] -> Maybe Content
parseSection obj = do
  title <- lookup "title" obj >>= getString
  contentArr <- lookup "content" obj >>= getArray
  contents <- mapM jsonValueToContent contentArr
  return (Section (Just title) contents)

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