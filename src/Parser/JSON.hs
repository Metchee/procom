module Parser.JSON where

import Parser.Core
import Document.Types
import Control.Applicative (many, some, (<|>))
import Data.Char (isSpace)
import Data.Maybe (catMaybes)

-- Parser pour les valeurs JSON
data JSONValue
  = JSONString String
  | JSONNumber Int
  | JSONObject [(String, JSONValue)]
  | JSONArray [JSONValue]
  | JSONBool Bool
  | JSONNull
  deriving (Show, Eq)

-- Parsers pour les différents types JSON
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
parseJSONObject = do
  char '{'
  spaces
  pairs <- parsePair `sepBy` (char ',' >> spaces)
  spaces
  char '}'
  return $ JSONObject pairs
  where
    parsePair = do
      key <- quotedString
      spaces
      char ':'
      spaces
      value <- parseJSONValue
      return (key, value)

-- Séparateur pour les listes
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = ((:) <$> p <*> many (sep >> p)) <|> pure []

-- Conversion d'un objet JSON en Document
jsonToDocument :: JSONValue -> Maybe Document
jsonToDocument (JSONObject obj) = do
  header <- getHeader obj
  body <- getBody obj
  return $ Document header body
  where
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

    getObject :: JSONValue -> Maybe [(String, JSONValue)]
    getObject (JSONObject o) = Just o
    getObject _ = Nothing

    getArray :: JSONValue -> Maybe [JSONValue]
    getArray (JSONArray arr) = Just arr
    getArray _ = Nothing

    getString :: JSONValue -> Maybe String
    getString (JSONString s) = Just s
    getString _ = Nothing

-- Conversion d'une valeur JSON en Content
jsonValueToContent :: JSONValue -> Maybe Content
jsonValueToContent (JSONString s) = Just (Text s)
jsonValueToContent (JSONObject obj) = case lookup "type" obj of
  Just (JSONString "paragraph") -> do
    content <- lookup "content" obj >>= getArray
    contentList <- mapM jsonValueToContent content
    return $ Paragraph contentList
  Just (JSONString "section") -> do
    title <- lookup "title" obj >>= getString
    content <- lookup "content" obj >>= getArray
    contentList <- mapM jsonValueToContent content
    return $ Section title contentList
  Just (JSONString "italic") -> do
    content <- lookup "content" obj >>= jsonValueToContent
    return $ Italic content
  Just (JSONString "bold") -> do
    content <- lookup "content" obj >>= jsonValueToContent
    return $ Bold content
  Just (JSONString "code") -> do
    code <- lookup "content" obj >>= getString
    return $ Code code
  Just (JSONString "link") -> do
    text <- lookup "text" obj >>= getString
    url <- lookup "url" obj >>= getString
    return $ Link text url
  Just (JSONString "image") -> do
    alt <- lookup "alt" obj >>= getString
    url <- lookup "url" obj >>= getString
    return $ Image alt url
  Just (JSONString "codeblock") -> do
    code <- lookup "content" obj >>= getString
    return $ CodeBlock code
  Just (JSONString "list") -> do
    items <- lookup "items" obj >>= getArray
    itemsList <- mapM jsonValueToItem items
    return $ List itemsList
  _ -> Nothing
  where
    getArray (JSONArray arr) = Just arr
    getArray _ = Nothing

    getString (JSONString s) = Just s
    getString _ = Nothing

jsonValueToItem :: JSONValue -> Maybe Item
jsonValueToItem (JSONObject obj) = do
  content <- lookup "content" obj >>= getArray
  contentList <- mapM jsonValueToContent content
  return $ Item contentList
  where
    getArray (JSONArray arr) = Just arr
    getArray _ = Nothing

-- Parse un document JSON à partir d'une chaîne
parseJSON :: String -> Maybe Document
parseJSON input = case runParser parseJSONValue input of
  Just (value, "") -> jsonToDocument value
  Just (value, rest) | all isSpace rest -> jsonToDocument value
  _ -> Nothing