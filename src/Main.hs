import System.Environment
import System.IO (stdout, stderr, hPutStr, hPutStrLn)
import Parsing
import Unparsing
import Interpreter

main = do
  [fileName] <- getArgs
  srcText <- readFile fileName
  case parseFile srcText of
    Left   err -> print err -- error
    Right prog -> print (exec' prog) --(unparseConstList' (exec' prog) [])
