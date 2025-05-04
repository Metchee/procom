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
  -- D'abord, essayons d'analyser le JSON normalement
  case runParser parseJSONValue input of
    Just (value, rest) | all isSpace rest -> jsonToDocument value
    -- Si ça échoue, vérifions si c'est un JSON "stringifié"
    _ -> case parseStringifiedJSON input of
           Just doc -> Just doc
           Nothing -> Nothing

-- Parser pour le JSON "stringifié" (quand le JSON est encapsulé dans une chaîne)
parseStringifiedJSON :: String -> Maybe Document
parseStringifiedJSON input = 
  -- Cas particulier pour les chaînes qui ressemblent à du JSON brut
  if "[(" `isPrefixOf` input || "{" `isPrefixOf` input then
    Just (Document (Header "" Nothing Nothing) [Text input])
  else 
    Nothing

jsonToDocument :: JSONValue -> Maybe Document
jsonToDocument (JSONObject obj) = do
  header <- getHeader obj
  body <- getBody obj
  return $ Document header body
jsonToDocument _ = Nothing

jsonValueToContent :: JSONValue -> Maybe Content
jsonValueToContent (JSONString s) = 
  -- Cas spécial pour les chaînes qui contiennent du JSON brut
  if "[(" `isPrefixOf` s || "{" `isPrefixOf` s then
    Just (Text s)
  else
    Just (Text s)
jsonValueToContent (JSONArray arr) = 
  let contents = catMaybes (map jsonValueToContent arr)
  in Just (Paragraph contents)
jsonValueToContent (JSONObject obj) =
  parseSpecialObject obj <|> parseStandardObject obj <|> Just (Text (show obj))

parseSpecialObject :: [(String, JSONValue)] -> Maybe Content
parseSpecialObject obj =
  parseItalic obj <|> parseBold obj <|> parseCode obj <|> 
  parseLink obj <|> parseImage obj <|> parseList obj <|> parseSection obj

parseItalic :: [(String, JSONValue)] -> Maybe Content
parseItalic obj = do
  content <- lookup "italic" obj >>= getString
  return (Paragraph [Italic (Text content)])

parseBold :: [(String, JSONValue)] -> Maybe Content
parseBold obj = do
  content <- lookup "bold" obj >>= getString
  return (Paragraph [Bold (Text content)])

parseCode :: [(String, JSONValue)] -> Maybe Content
parseCode obj = do
  content <- lookup "code" obj >>= getString
  return (Paragraph [Code content])

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

parseList :: [(String, JSONValue)] -> Maybe Content
parseList obj = do
  guard $ lookup "type" obj == Just (JSONString "list")
  items <- lookup "items" obj >>= getArray
  itemsList <- mapM parseListItem items
  return (List itemsList)

parseListItem :: JSONValue -> Maybe Item
parseListItem (JSONObject obj) = do
  contentArr <- lookup "content" obj >>= getArray
  contents <- mapM jsonValueToContent contentArr
  return (Item contents)
parseListItem _ = Nothing

parseSection :: [(String, JSONValue)] -> Maybe Content
parseSection obj = do
  guard $ lookup "type" obj == Just (JSONString "section")
  title <- lookup "title" obj >>= getString
  contentArr <- lookup "content" obj >>= getArray
  contents <- mapM jsonValueToContent contentArr
  return (Section title contents)

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