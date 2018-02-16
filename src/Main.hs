import System.Environment
import System.IO (stdout, stderr, hPutStr, hPutStrLn)
import Parsing 

main = do
  [fileName] <- getArgs
  srcText <- readFile fileName
  case parseFile srcText of
    Left e -> print e -- error
    Right r -> print r
