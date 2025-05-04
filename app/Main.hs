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

parseOptions :: IO Options
parseOptions = execParser optsInfo
  where
    optsInfo = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Convert documents between different formats"
     <> header "mypandoc - a document converter" )

main :: IO ()
main = do
  opts <- parseOptions
  safeRun opts

handleError :: IOException -> IO ()
handleError e = 
  hPutStrLn stderr ("Error: " ++ show e) >>
  exitWith (ExitFailure 84)

safeRun :: Options -> IO ()
safeRun opts = do
  result <- try (run opts)
  case result of
    Left e -> handleError e
    Right _ -> return ()

run :: Options -> IO ()
run opts = do
  validateOptions opts
  content <- readFileOrExit (inputFile opts)
  format <- determineFormat opts content
  doc <- parseDocumentSafe format content
  writeOutput opts doc

validateFormat :: String -> Bool -> IO ()
validateFormat fmt valid =
  if not valid then exitWithError (UnsupportedFormat fmt) else return ()

validateOptions :: Options -> IO ()
validateOptions opts = do
  validateFormat (outputFormat opts) (isValidFormat (outputFormat opts))
  case inputFormat opts of
    Just fmt -> validateFormat fmt (isValidFormat fmt)
    Nothing -> return ()

determineFormat :: Options -> String -> IO Format
determineFormat opts content = case inputFormat opts of
  Just fmt -> return (stringToFormat fmt)
  Nothing -> case detectFormat content of
    Just fmt -> return fmt
    Nothing -> return XML

parseDocumentSafe :: Format -> String -> IO Document
parseDocumentSafe format content = do
  let result = parseByFormat format content
  case result of
    Just d -> return d
    Nothing -> do
      -- Print debugging info
      hPutStrLn stderr $ "Debug: Trying to parse " ++ show format ++ " failed"
      case parseAuto content of
        Just d -> return d
        Nothing -> do
          hPutStrLn stderr $ "Debug: Auto parse also failed"
          case format of
            XML -> hPutStrLn stderr "Debug: XML parsing failed"
            JSON -> hPutStrLn stderr "Debug: JSON parsing failed"
            Markdown -> hPutStrLn stderr "Debug: Markdown parsing failed"
          exitWithError (ParseError "Failed to parse document")

handleWriteError :: IOException -> IO ()
handleWriteError e = 
  hPutStrLn stderr $ "Error when writing to file: " ++ show e

writeToFile :: FilePath -> String -> IO ()
writeToFile path content = do
  result <- E.try (writeFile path content)
  case result of
    Left e -> handleWriteError e
    Right _ -> return ()

writeOutput :: Options -> Document -> IO ()
writeOutput opts doc =
  let output = formatByFormat (stringToFormat (outputFormat opts)) doc
  in case outputFile opts of
       Just file -> writeToFile file output
       Nothing   -> putStr output

handleReadError :: IOException -> IO String
handleReadError e = do
  hPutStrLn stderr ("Error when reading file: " ++ show e)
  exitWithError (FileNotFound (show e))

readFileOrExit :: FilePath -> IO String
readFileOrExit path = E.catch (readFile path) handleReadError

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