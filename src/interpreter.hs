import System.Environment
import System.IO (stdout,stderr,hPutStr,hPutStrLn)
import Grammar


main = do
  [fileName] <- getArgs
  hPutStr stdout (fileName)
