import System.Environment
import System.IO (stdout,stderr,hPutStr,hPutStrLn)
import Interpreter

main = do
  [fileName] <- getArgs
  srcText <- readFile fileName
  {- hPutStr stdout (unlines (matching (lines srcText))) -}
  case (matching (lines srcText)) of
       True -> hPutStr stdout "True"
       _    -> hPutStr stdout "False"
