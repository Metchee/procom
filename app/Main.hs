{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (when)
import System.IO (readFile, writeFile, hPutStrLn, stderr)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import Control.Exception (IOException)  -- Ce type est disponible via base
import qualified Control.Exception as E -- On n'utilisera que le type, pas les fonctions

import Document.Types
import Parser.XML (parseXML)
import Parser.JSON (parseJSON)
import Parser.Markdown (parseMarkdown)
import Parser.Detect (detectFormat, parseByFormat, Format(..))
import Formatter.XML (formatXML)
import Formatter.JSON (formatJSON)
import Formatter.Markdown (formatMarkdown)
import Error

-- Structure pour les options de ligne de commande
data Options = Options
  { inputFile  :: FilePath
  , outputFormat :: String
  , outputFile :: Maybe FilePath
  , inputFormat :: Maybe String
  }

-- Parser pour les options
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long "input"
     <> short 'i'
     <> metavar "FILE"
     <> help "Path to input file" )
  <*> strOption
      ( long "format"
     <> short 'f'
     <> metavar "FORMAT"
     <> help "Output format (xml, json, markdown)" )
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> help "Path to output file" ))
  <*> optional (strOption
      ( long "input-format"
     <> short 'e'
     <> metavar "FORMAT"
     <> help "Input format (xml, json, markdown)" ))

-- Fonction principale
main :: IO ()
main = do
  opts <- execParser optsInfo
  run opts
  where
    optsInfo = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Convert documents between different formats"
     <> header "mypandoc - a document converter" )

-- Exécuter le programme avec les options fournies
run :: Options -> IO ()
run opts = do
  -- Vérifier le format de sortie
  unless (isValidFormat (outputFormat opts)) $
    exitWithError (UnsupportedFormat (outputFormat opts))
  
  -- Vérifier le format d'entrée s'il est spécifié
  when (isJust (inputFormat opts) && not (isValidFormat (fromJust (inputFormat opts)))) $
    exitWithError (UnsupportedFormat (fromJust (inputFormat opts)))
  
  -- Lire le fichier d'entrée
  content <- readFileOrExit (inputFile opts)
  
  -- Détecter le format d'entrée ou utiliser celui spécifié
  format <- case inputFormat opts of
    Just fmt -> return (stringToFormat fmt)
    Nothing -> case detectFormat content of
      Just fmt -> return fmt
      Nothing -> exitWithError (ParseError "Could not detect input format")
  
  -- Parser le document
  doc <- case parseByFormat format content of
    Just d -> return d
    Nothing -> exitWithError (ParseError "Failed to parse document")
  
  -- Formatter et sortir le résultat
  let output = formatByFormat (stringToFormat (outputFormat opts)) doc
  case outputFile opts of
    Just file -> writeFile file output
    Nothing -> putStr output

-- Vérifier si un format est valide
isValidFormat :: String -> Bool
isValidFormat "xml" = True
isValidFormat "json" = True
isValidFormat "markdown" = True
isValidFormat _ = False

-- Convertir une chaîne en Format
stringToFormat :: String -> Format
stringToFormat "xml" = XML
stringToFormat "json" = JSON
stringToFormat "markdown" = Markdown
stringToFormat _ = error "Invalid format"

-- Formatter un document selon le format spécifié
formatByFormat :: Format -> Document -> String
formatByFormat XML = formatXML
formatByFormat JSON = formatJSON
formatByFormat Markdown = formatMarkdown

-- Lire un fichier avec gestion d'erreur - version simplifiée
readFileOrExit :: FilePath -> IO String
readFileOrExit path = do
  -- À la place de try/catch, on va simplement essayer de lire le fichier
  -- Si une erreur se produit, le programme s'arrêtera avec un message personnalisé
  catch (readFile path) (\(_ :: IOException) -> 
    exitWithError (FileNotFound path))

-- Fonction basique de gestion d'erreur pour les IO actions
catch :: IO a -> (IOException -> IO a) -> IO a
catch action handler = E.catch action handler

-- Fonctions utilitaires
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"

unless :: Bool -> IO () -> IO ()
unless p m = if p then pure () else m