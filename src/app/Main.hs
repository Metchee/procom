{-
-- EPITECH PROJECT, 2025
-- procom
-- File description:
-- Main
-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (when)
import System.IO (readFile, writeFile, hPutStrLn, stderr)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import Control.Exception (IOException, catch, try)
import qualified Control.Exception as E
import Document.Types
import Parser.XML (parseXML)
import Parser.JSON (parseJSON)
import Parser.Markdown (parseMarkdown)
import Parser.Detect (detectFormat, parseByFormat, Format(..), parseAuto)
import Formatter.XML (formatXML)
import Formatter.JSON (formatJSON)
import Formatter.Markdown (formatMarkdown)
import Error

data Options = Options
  { inputFile  :: FilePath
  , outputFormat :: String
  , outputFile :: Maybe FilePath
  , inputFormat :: Maybe String
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> inputFileOption
  <*> outputFormatOption
  <*> outputFileOption
  <*> inputFormatOption

inputFileOption :: Parser FilePath
inputFileOption = strOption
  ( long "input"
 <> short 'i'
 <> metavar "FILE"
 <> help "Path to input file" )

outputFormatOption :: Parser String
outputFormatOption = strOption
  ( long "format"
 <> short 'f'
 <> metavar "FORMAT"
 <> help "Output format (xml, json, markdown)" )

outputFileOption :: Parser (Maybe FilePath)
outputFileOption = optional (strOption
  ( long "output"
 <> short 'o'
 <> metavar "FILE"
 <> help "Path to output file" ))

inputFormatOption :: Parser (Maybe String)
inputFormatOption = optional (strOption
  ( long "input-format"
 <> short 'e'
 <> metavar "FORMAT"
 <> help "Input format (xml, json, markdown)" ))

main :: IO ()
main = do
  opts <- execParser optsInfo
  safeRun opts
  where
    optsInfo = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Convert documents between different formats"
     <> header "mypandoc - a document converter" )

safeRun :: Options -> IO ()
safeRun opts = 
  (try (run opts) :: IO (Either IOException ())) >>= \result ->
  case result of
    Left e -> handleError e
    Right _ -> return ()
  where
    handleError :: IOException -> IO ()
    handleError e = 
      hPutStrLn stderr ("Erreur: " ++ show e) >>
      exitWith (ExitFailure 84)

run :: Options -> IO ()
run opts =
  validateOptions opts >>
  readFileOrExit (inputFile opts) >>= \content ->
  determineFormat opts content >>= \format ->
  parseDocument format content >>= \doc ->
  writeOutput opts doc

validateOptions :: Options -> IO ()
validateOptions opts =
  case outputFormat opts of
    fmt | not (isValidFormat fmt) -> exitWithError (UnsupportedFormat fmt)
    _ -> case inputFormat opts of
           Just fmt | not (isValidFormat fmt) -> 
             exitWithError (UnsupportedFormat fmt)
           _ -> return ()

determineFormat :: Options -> String -> IO Format
determineFormat opts content = case inputFormat opts of
  Just fmt -> return (stringToFormat fmt)
  Nothing -> case detectFormat content of
    Just fmt -> return fmt
    Nothing -> return XML

parseDocument :: Format -> String -> IO Document
parseDocument format content = case parseByFormat format content of
  Just d -> return d
  Nothing -> case parseAuto content of
      Just d -> return d
      Nothing -> return $ Document (Header "" Nothing Nothing) []

writeOutput :: Options -> Document -> IO ()
writeOutput opts doc =
  let output = formatByFormat (stringToFormat (outputFormat opts)) doc
  in case outputFile opts of
       Just file -> safeWriteFile file output
       Nothing   -> putStr output

safeWriteFile :: FilePath -> String -> IO ()
safeWriteFile path content = 
  (E.try (writeFile path content) :: IO (Either IOException ())) >>= \result ->
  case result of
    Left e -> hPutStrLn stderr $ "Erreur lors de l'écriture du fichier: " 
              ++ show e
    Right _ -> return ()

isValidFormat :: String -> Bool
isValidFormat "xml" = True
isValidFormat "json" = True
isValidFormat "markdown" = True
isValidFormat "md" = True
isValidFormat _ = False

stringToFormat :: String -> Format
stringToFormat "xml" = XML
stringToFormat "json" = JSON
stringToFormat "markdown" = Markdown
stringToFormat "md" = Markdown
stringToFormat _ = XML

formatByFormat :: Format -> Document -> String
formatByFormat XML = formatXML
formatByFormat JSON = formatJSON
formatByFormat Markdown = formatMarkdown

readFileOrExit :: FilePath -> IO String
readFileOrExit path = E.catch (readFile path) handleReadError
  where
    handleReadError :: IOException -> IO String
    handleReadError e = 
      hPutStrLn stderr ("Erreur lors de la lecture du fichier: " ++ 
                        show e) >>
      return ""

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"

unless :: Bool -> IO () -> IO ()
unless p m = if p then pure () else m