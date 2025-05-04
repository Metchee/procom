{-
-- EPITECH PROJECT, 2025
-- procom
-- File description:
-- Error
-}

module Error where

import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)

data MyPandocError
  = InvalidArguments String
  | FileNotFound String
  | ParseError String
  | UnsupportedFormat String
  | OutputError String
  deriving (Show, Eq)

exitWithError :: MyPandocError -> IO a
exitWithError err = 
  hPutStrLn stderr (formatError err) >>
  exitWith (ExitFailure 84)
  
formatError :: MyPandocError -> String
formatError (InvalidArguments msg) = "Error: Invalid arguments - " ++ msg
formatError (FileNotFound path) = "Error: File not found - " ++ path
formatError (ParseError msg) = "Error: Parse error - " ++ msg
formatError (UnsupportedFormat fmt) = "Error: Unsupported format - " ++ fmt
formatError (OutputError msg) = "Error: Output error - " ++ msg

showUsage :: IO ()
showUsage =
  putStrLn "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]" >>
  putStrLn "  ifile       path to the file to convert" >>
  putStrLn "  oformat     output format (xml, json, markdown)" >>
  putStrLn "  ofile       path to the output file" >>
  putStrLn "  iformat     input format (xml, json, markdown)"