module Error where

import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)

-- Types d'erreurs
data MyPandocError
  = InvalidArguments String
  | FileNotFound String
  | ParseError String
  | UnsupportedFormat String
  | OutputError String
  deriving (Show, Eq)

-- Afficher un message d'erreur et quitter avec le code 84
exitWithError :: MyPandocError -> IO a
exitWithError err = do
  hPutStrLn stderr (formatError err)
  exitWith (ExitFailure 84)  -- Utilisez ExitFailure au lieu de ExitCode
  
-- Formater un message d'erreur
formatError :: MyPandocError -> String
formatError (InvalidArguments msg) = "Error: Invalid arguments - " ++ msg
formatError (FileNotFound path) = "Error: File not found - " ++ path
formatError (ParseError msg) = "Error: Parse error - " ++ msg
formatError (UnsupportedFormat fmt) = "Error: Unsupported format - " ++ fmt
formatError (OutputError msg) = "Error: Output error - " ++ msg

-- Afficher un message d'usage
showUsage :: IO ()
showUsage = do
  putStrLn "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]"
  putStrLn "  ifile       path to the file to convert"
  putStrLn "  oformat     output format (xml, json, markdown)"
  putStrLn "  ofile       path to the output file"
  putStrLn "  iformat     input format (xml, json, markdown)"