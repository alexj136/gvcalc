import Util
import Lexer
import Parser
import Syntax
import Types
import Interpreter

import System.Environment (getArgs)
import Control.Monad.Except (throwError)
import qualified Data.Map as M

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        putStrLn "Please supply a single filename."
    else do
        sourceCode <- readFile (head args)
        putStrLn $ case runGVCalc (runChain sourceCode) (Name 0, M.empty) of
            Left   err     -> err
            Right (str, _) -> str

runChain :: String -> GVCalc String
runChain sourceCode = do
    tokens   <- scan  sourceCode
    ast      <- parse tokens
    machine  <- fromConfig ast
    machine' <- run machine
    pp (toConfig machine')
