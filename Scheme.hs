{-# OPTIONS_GHC -Wall #-} 
-- {-# LANGUAGE ExistentialQuantification #-}

import Val
import Eval

import Prelude hiding (pred)
import Data.IORef (newIORef)
import System.Environment (getArgs)
import System.IO (hPutStrLn, hFlush, stdout, stderr)
import Control.Monad.Error (ErrorT (..), liftM, catchError)

nullEnv :: IO Env
nullEnv = newIORef []

runIOThrows :: ErrorT LispError IO String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

trapError :: ErrorT LispError IO String -> ErrorT LispError IO String 
trapError action = catchError action (return . show)

extractValue :: Either LispError a -> a
extractValue (Right val) = val
-- this is undefined on purpose, this will hopefully never be reached
extractValue (Left _) = undefined

primitiveBindings :: IO Env
-- primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimFunc prims)
--     where makePrimFunc (var, func) = (var, PrimFuncVal func)
primitiveBindings = nullEnv >>= 
    (flip bindVars $ map (makeFunc IOFuncVal) ioPrims ++ map (makeFunc PrimFuncVal) prims)
    where makeFunc constructor (var, func) = (var, constructor func)


{-
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ NoParse err
    Right val -> return val
    -}


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

-- i don't know why that other version of evalString uses the IO monad... 
-- evalString :: String -> String
-- evalString = extractValue . trapError . liftM show . (readExpr >=> eval)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>= 
            until_ (== "quit") (readPrompt "Lisp> ") . evalAndPrint

-- runOne :: String -> IO ()
-- runOne expr = primitiveBindings >>= flip evalAndPrint expr
runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= 
        flip bindVars [("args", ListVal $ map StringVal $ drop 1 args)]
    (runIOThrows $ liftM show 
        $ eval env (ListVal [AtomVal "load", StringVal (head args)])) 
        >>= hPutStrLn stderr

main :: IO ()
main = do
    args <- getArgs
    if null args then runRepl else runOne $ args
