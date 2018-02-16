import System.Environment
import System.IO (stdout, stderr, hPutStr, hPutStrLn)
import Parsing 
import Interpreter

main = do
  [fileName] <- getArgs
  srcText <- readFile fileName
  case parseFile srcText of
    Left   err -> print err -- error
    Right prog -> print (exec' prog)
