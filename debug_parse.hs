-- debug_parse.hs
-- Compile with: ghc -o debug_parse debug_parse.hs

module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Document.Types
import Parser.Detect (detectFormat, parseByFormat, Format(..))
import Formatter.XML (formatXML)
import Formatter.JSON (formatJSON)
import Formatter.Markdown (formatMarkdown)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      content <- readFile file
      case detectFormat content of
        Just format -> do
          putStrLn $ "Detected format: " ++ show format
          case parseByFormat format content of
            Just doc -> do
              putStrLn "Document parsed successfully:"
              putStrLn $ show doc
              putStrLn "\nFormatted as XML:"
              putStrLn $ formatXML doc
              putStrLn "\nFormatted as JSON:"
              putStrLn $ formatJSON doc
              putStrLn "\nFormatted as Markdown:"
              putStrLn $ formatMarkdown doc
            Nothing -> putStrLn "Failed to parse document"
        Nothing -> putStrLn "Could not detect format"
    _ -> putStrLn "Usage: debug_parse <file>"
