import Tokens
import System.IO
import Grammar
import System.Environment
import Control.Exception

main :: IO ()
main = catch lexer handler

lexer :: IO ()
lexer = do
    (fileName : _) <- getArgs
    contents <- readFile fileName
    putStrLn $ show $ parseSQL $ alexScanTokens contents

handler :: ErrorCall -> IO ()
handler e = do
    let error = show e
    putStrLn ("Error " ++ error)