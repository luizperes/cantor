import System.Environment
import System.IO (stdout, stderr, hPutStr, hPutStrLn)
import Parsing
import Unparsing
import Interpreter

main = do
  [fileName] <- getArgs
  srcText <- readFile fileName
  case parseFile srcText of
    Left   err -> print err
    Right prog -> putStrLn $ id (unparseConstList' (exec' prog) [])
