import System.Environment
import System.IO (stdout,stderr,hPutStr,hPutStrLn)
import Interpreter

main = do
  [fileName] <- getArgs
  srcText <- readFile fileName
  parseFile srcText
