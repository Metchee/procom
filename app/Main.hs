{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (when)
import System.IO (readFile, writeFile, hPutStrLn, stderr)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import Control.Exception (IOException)
import qualified Control.Exception as E

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
  hPutStrLn stderr $ "Lecture du fichier: " ++ inputFile opts
  content <- readFileOrExit (inputFile opts)
  
  -- Détecter le format d'entrée ou utiliser celui spécifié
  format <- case inputFormat opts of
    Just fmt -> do
      hPutStrLn stderr $ "Format spécifié: " ++ fmt
      return (stringToFormat fmt)
    Nothing -> do
      case detectFormat content of
        Just fmt -> do
          hPutStrLn stderr $ "Format détecté: " ++ show fmt
          return fmt
        Nothing -> do
          hPutStrLn stderr "Impossible de détecter le format d'entrée"
          exitWithError (ParseError "Could not detect input format")
  
  -- Parser le document
  hPutStrLn stderr $ "Tentative de parsing avec le format: " ++ show format
  hPutStrLn stderr $ "Contenu (premières 100 caractères): " ++ take 100 content
  
  doc <- case parseByFormat format content of
    Just d -> do
      hPutStrLn stderr "Parsing réussi!"
      return d
    Nothing -> do
      hPutStrLn stderr "Échec du parsing"
      exitWithError (ParseError "Failed to parse document")
  
  -- Formatter et sortir le résultat
  hPutStrLn stderr $ "Formatage au format: " ++ outputFormat opts
  let output = formatByFormat (stringToFormat (outputFormat opts)) doc
  case outputFile opts of
    Just file -> do
      hPutStrLn stderr $ "Écriture dans le fichier: " ++ file
      writeFile file output
    Nothing -> putStr output
  
  hPutStrLn stderr "Conversion terminée avec succès"

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

-- Lire un fichier avec gestion d'erreur
readFileOrExit :: FilePath -> IO String
readFileOrExit path = do
  catch (readFile path) (\(e :: IOException) -> do
    hPutStrLn stderr $ "Erreur lors de la lecture du fichier: " ++ show e
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