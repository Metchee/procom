module Parser.XML where

import Parser.Core
import Document.Types
import Control.Applicative (many, some, (<|>), optional)
import Data.Char (isSpace, isAlpha)
import Data.Maybe (catMaybes, fromMaybe)

-- Parser pour un document XML simple mais robuste
parseXMLDocument :: Parser Document
parseXMLDocument = do
  spaces
  string "<document>"
  spaces
  header <- parseHeader
  spaces
  body <- parseBody
  spaces
  string "</document>"
  spaces
  return $ Document header body

-- Parser pour l'en-tête
parseHeader :: Parser Header
parseHeader = do
  string "<header"
  spaces
  attrs <- many parseAttribute
  spaces
  string ">"
  spaces
  string "</header>"
  
  let title = fromMaybe "" (lookup "title" attrs)
  let author = lookup "author" attrs
  let date = lookup "date" attrs
  
  return $ Header title author date

-- Parser pour un attribut
parseAttribute :: Parser (String, String)
parseAttribute = do
  name <- some (satisfy (\c -> isAlpha c || c == '_'))
  spaces
  char '='
  spaces
  char '"'
  value <- many (satisfy (/= '"'))
  char '"'
  spaces
  return (name, value)

-- Parser pour le corps
parseBody :: Parser [Content]
parseBody = do
  string "<body>"
  spaces
  contents <- many parseContent
  spaces
  string "</body>"
  return contents

-- Parser pour les différents types de contenu
parseContent :: Parser Content
parseContent = parseParagraph 
           <|> parseSection 
           <|> parseList
           <|> parseText
           <|> parseBold
           <|> parseItalic
           <|> parseCode
           <|> parseCodeBlock
           <|> parseLink
           <|> parseImage

-- Parser pour un paragraphe
parseParagraph :: Parser Content
parseParagraph = do
  string "<paragraph>"
  spaces
  contents <- many parseContent
  spaces
  string "</paragraph>"
  spaces
  return $ Paragraph contents

-- Parser pour une section
parseSection :: Parser Content
parseSection = do
  string "<section"
  spaces
  attrs <- many parseAttribute
  spaces
  string ">"
  spaces
  contents <- many parseContent
  spaces
  string "</section>"
  spaces
  
  let title = fromMaybe "" (lookup "title" attrs)
  return $ Section title contents

-- Parser pour une liste
parseList :: Parser Content
parseList = do
  string "<list>"
  spaces
  items <- many parseItem
  spaces
  string "</list>"
  spaces
  return $ List items

-- Parser pour un item de liste
parseItem :: Parser Item
parseItem = do
  string "<item>"
  spaces
  contents <- many parseContent
  spaces
  string "</item>"
  spaces
  return $ Item contents

-- Parser pour du texte simple
parseText :: Parser Content
parseText = do
  text <- some (satisfy (\c -> c /= '<' && c /= '>'))
  return $ Text text

-- Parser pour du texte en gras
parseBold :: Parser Content
parseBold = do
  string "<bold>"
  spaces
  content <- parseContentInline
  spaces
  string "</bold>"
  spaces
  return $ Bold content

-- Parser pour du texte en italique
parseItalic :: Parser Content
parseItalic = do
  string "<italic>"
  spaces
  content <- parseContentInline
  spaces
  string "</italic>"
  spaces
  return $ Italic content

-- Parser pour du code en ligne
parseCode :: Parser Content
parseCode = do
  string "<code>"
  code <- many (satisfy (/= '<'))
  string "</code>"
  spaces
  return $ Code code

-- Parser pour un bloc de code
parseCodeBlock :: Parser Content
parseCodeBlock = do
  string "<codeblock>"
  spaces
  code <- many (satisfy (\c -> c /= '<' || not (isPrefixOf "</codeblock>" (c:remainingInput))))
  string "</codeblock>"
  spaces
  return $ CodeBlock code
  where
    isPrefixOf _ [] = False
    isPrefixOf [] _ = True
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    
    remainingInput = "" -- Cette ligne est un placeholder, remplaçable par une implémentation plus robuste

-- Parser pour un lien
parseLink :: Parser Content
parseLink = do
  string "<link"
  spaces
  attrs <- many parseAttribute
  spaces
  string ">"
  spaces
  string "</link>"
  spaces
  
  let text = fromMaybe "" (lookup "text" attrs)
  let url = fromMaybe "" (lookup "url" attrs)
  return $ Link text url

-- Parser pour une image
parseImage :: Parser Content
parseImage = do
  string "<image"
  spaces
  attrs <- many parseAttribute
  spaces
  string ">"
  spaces
  string "</image>"
  spaces
  
  let alt = fromMaybe "" (lookup "alt" attrs)
  let url = fromMaybe "" (lookup "url" attrs)
  return $ Image alt url

-- Parser pour le contenu inline (texte simple pour l'instant)
parseContentInline :: Parser Content
parseContentInline = 
  parseText <|> 
  (do
    string "<bold>"
    content <- parseContentInline
    string "</bold>"
    return $ Bold content) <|>
  (do
    string "<italic>"
    content <- parseContentInline
    string "</italic>"
    return $ Italic content) <|>
  (do
    string "<code>"
    code <- many (satisfy (/= '<'))
    string "</code>"
    return $ Code code)

-- Parse un document XML à partir d'une chaîne
parseXML :: String -> Maybe Document
parseXML input = case runParser parseXMLDocument input of
  Just (doc, rest) | all isSpace rest -> Just doc
  _ -> Nothing