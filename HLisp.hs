import HLisp.Builtin
import HLisp.Environment
import HLisp.Evaluator
import HLisp.Reader
import HLisp.SExpr
import System.IO
import System.Environment
import Control.Monad
import Control.Monad.State
import Control.Exception (try)
import System.IO.Error (isEOFError)

main = do
    args <- getArgs
    fileContents <- mapM readFile args
    let (result, env) = runState (handleAllFiles fileContents) defaultEnvironment
    interactive env
        
interactive env = do
    maybeInput <- try (getLine)
    case maybeInput of
        Left e -> if isEOFError e then return () else ioError e
        Right input -> do
            let result = readProgram input
            case readExpression input of
                Left err -> do 
                    putStrLn $ show err
                    interactive env
                Right expr -> do
                    let (result, newEnv) = runState (eval expr) env
                    putStrLn $ show result
                    interactive newEnv

handleAllFiles inputs = mapM handleOneFile inputs

handleOneFile input =
    case readProgram input of
        Left  err   -> fail $ show err
        Right exprs -> evalAll exprs
