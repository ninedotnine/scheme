-- {-# OPTIONS_GHC -Wall #-} 
{-# LANGUAGE ExistentialQuantification #-}
module Eval (
    eval,
    readExpr,
    bindVars,
    ioPrims,
    prims,
    liftThrows,
) where

import Val

import Prelude hiding (pred)
import Data.IORef (readIORef, newIORef, writeIORef)
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO (IOMode(..), hPrint, hGetLine, openFile, hClose, stdout,stdin)
import Control.Monad.Error (ErrorT, liftM, liftIO, throwError, catchError)
import Numeric (readFloat)

eval :: Env -> LispVal -> ErrorT LispError IO LispVal
eval _ val@(StringVal _) = return val
eval _ val@(IntVal _)    = return val
eval _ val@(BoolVal _)   = return val
eval _ val@(FloatVal _)  = return val
eval env (AtomVal name)  = getVar env name
eval _ (ListVal [AtomVal "quote", val]) = return val
eval env (ListVal [AtomVal "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        BoolVal False -> eval env alt
        _             -> eval env conseq
eval env (ListVal [AtomVal "set!", AtomVal var, form]) =
     eval env form >>= setVar env var
eval env (ListVal [AtomVal "define", AtomVal var, form]) =
     eval env form >>= defineVar env var
eval env (ListVal (AtomVal "define" : ListVal (AtomVal var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (ListVal (AtomVal "define" : DottedListVal (AtomVal var : params) varargs : body)) = 
    makeVarArgs varargs env params body >>= defineVar env var
eval env (ListVal (AtomVal "lambda" : ListVal params : body)) =
    makeNormalFunc env params body
eval env (ListVal (AtomVal "lambda" : DottedListVal params varargs : body)) =
    makeVarArgs varargs env params body
eval env (ListVal (AtomVal "lambda" : varargs@(AtomVal _) : body)) =
    makeVarArgs varargs env [] body
eval env (ListVal [AtomVal "load", StringVal filename]) = 
    load filename >>= liftM last . mapM (eval env)
eval env (ListVal (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval _ badForm = 
    throwError $ BadSpecialForm "unrecognized special form" badForm



prims :: [(String, [LispVal] -> Either LispError LispVal)]
prims = [("+", numericBinOp (+)),
         ("-", numericBinOp (-)),
         ("*", numericBinOp (*)),
         ("/", numericBinOp div),
         ("mod", numericBinOp mod),
         ("quotient", numericBinOp quot),
         ("remainder", numericBinOp rem),
         ("=", intBoolBinop (==)),
         ("<", intBoolBinop (<)),
         (">", intBoolBinop (>)),
         ("/=", intBoolBinop (/=)),
         (">=", intBoolBinop (>=)),
         ("<=", intBoolBinop (<=)),
         ("&&", boolBoolBinop (&&)),
         ("||", boolBoolBinop (||)),
         ("string=?", strBoolBinop (==)),
         ("string<?", strBoolBinop (<)),
         ("string>?", strBoolBinop (>)),
         ("string<=?", strBoolBinop (<=)),
         ("string>=?", strBoolBinop (>=)),
         ("car", car),
         ("cdr", cdr),
         ("cons", cons),
         ("eq?", eqv),
         ("eqv?", eqv),
         ("equal?", equal)
        ]

ioPrims :: [(String, [LispVal] -> ErrorT LispError IO LispVal)]
ioPrims = [("apply", applyProc),
           ("open-input-file", makePort ReadMode),
           ("open-output-file", makePort WriteMode),
           ("close-input-port", closePort),
           ("close-output-port", closePort),
           ("read", readProc),
           ("write", writeProc),
           ("read-contents", readContents),
           ("read-all", readAll)
          ]


car :: [LispVal] -> Either LispError LispVal
car [ListVal (x:_)]         = return x
car [DottedListVal (x:_) _] = return x
car [badArg]                 = throwError $ TypeMismatch "pair" badArg
car badArgList               = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> Either LispError LispVal
cdr [ListVal (_:xs)]         = return (ListVal xs)
cdr [DottedListVal [_] x]    = return x
cdr [DottedListVal (_:xs) x] = return (DottedListVal xs x)
cdr [badArg]                 = throwError $ TypeMismatch "pair" badArg
cdr badArgList               = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> Either LispError LispVal
cons [x, ListVal []] = return (ListVal [x])
cons [x, ListVal xs] = return (ListVal (x:xs))
cons [x, DottedListVal xs last] = return (DottedListVal (x:xs) last)
cons [x1, x2] = return (DottedListVal [x1] x2)
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> Either LispError LispVal
eqv [(BoolVal arg1), (BoolVal arg2)]       = return $ BoolVal (arg1 == arg2)
eqv [(IntVal arg1), (IntVal arg2)]         = return $ BoolVal (arg1 == arg2)
eqv [(StringVal arg1), (StringVal arg2)]   = return $ BoolVal (arg1 == arg2)
eqv [(AtomVal arg1), (AtomVal arg2)]       = return $ BoolVal (arg1 == arg2)
eqv [(DottedListVal xs x), (DottedListVal ys y)] = eqv [ListVal $ xs ++ [x], 
                                                      ListVal $ ys ++ [y]]
eqv [(ListVal arg1), (ListVal arg2)] = return $ BoolVal $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where eqvPair (x, y) = case eqv  [x, y] of
            Left _            -> False
            Right (BoolVal b) -> b
eqv [_,_] = return $ BoolVal False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> Either LispError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                         [AnyUnpacker unPackInt, 
                          AnyUnpacker unPackStr, 
                          AnyUnpacker unPackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ BoolVal $ (primitiveEquals || let (BoolVal x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList



numericBinOp :: (Integer -> Integer -> Integer) 
                -> [LispVal] -> Either LispError LispVal 
numericBinOp _ [] = throwError $ NumArgs 2 []
numericBinOp _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params = mapM unPackInt params >>= return . IntVal . foldl1 op 

boolBinop :: (LispVal -> Either LispError a) -> (a -> a -> Bool) 
             -> [LispVal] -> Either LispError LispVal
boolBinop unPacker op args = if length args /= 2
   then throwError $ NumArgs 2 args
   else do 
        [left, right] <- mapM unPacker args 
        return $ BoolVal (left `op` right)

intBoolBinop :: (Integer -> Integer -> Bool) 
                -> [LispVal] -> Either LispError LispVal 
intBoolBinop = boolBinop unPackInt

boolBoolBinop :: (Bool -> Bool -> Bool) 
                -> [LispVal] -> Either LispError LispVal 
boolBoolBinop = boolBinop unPackBool

strBoolBinop :: (String -> String -> Bool) 
                -> [LispVal] -> Either LispError LispVal 
strBoolBinop = boolBinop unPackStr

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> Either LispError a)

unPackBool :: LispVal -> Either LispError Bool
unPackBool (BoolVal b) = return b
unPackBool notBool     = throwError $ TypeMismatch "bool" notBool

unPackStr :: LispVal -> Either LispError String
unPackStr (StringVal str) = return str
unPackStr (IntVal n)      = return (show n)
unPackStr (BoolVal b)     = return (show b)
unPackStr notString       = throwError $ TypeMismatch "string" notString

unPackInt :: LispVal -> Either LispError Integer
unPackInt (IntVal n) = return n
unPackInt (StringVal str) = let parsed = reads str :: [(Integer, String)] in
    if null parsed
        then throwError $ TypeMismatch "integer" (StringVal str)
        else return . fst . head $ parsed
unPackInt (ListVal [n]) = unPackInt n
unPackInt notInt = throwError $ TypeMismatch "integer" notInt


getVar :: Env -> String -> ErrorT LispError IO LispVal
getVar envRef var = do 
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> ErrorT LispError IO LispVal
setVar envRef var value = do 
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef value)) (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> ErrorT LispError IO LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value -- unnecessary return?
--         then setVar envRef var value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

applyProc :: [LispVal] -> ErrorT LispError IO LispVal
applyProc [func, ListVal args] = apply func args
applyProc (func:args)          = apply func args
-- applyProc []                   = throwError $ NumArgs 2 []

makePort :: IOMode -> [LispVal] -> ErrorT LispError IO LispVal
makePort mode [StringVal name] = liftM PortVal $ liftIO $ openFile name mode

closePort :: [LispVal] -> ErrorT LispError IO LispVal 
closePort [PortVal port] = liftIO $ hClose port >> (return (BoolVal True))
closePort _              = return (BoolVal False)

writeProc :: [LispVal] -> ErrorT LispError IO LispVal
writeProc [obj]               = writeProc [obj, PortVal stdout]
writeProc [obj, PortVal port] = liftIO (hPrint port obj) >> (return (BoolVal True))

readProc :: [LispVal] -> ErrorT LispError IO LispVal 
readProc [] = readProc [PortVal stdin]
readProc [PortVal port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

readContents :: [LispVal] -> ErrorT LispError IO LispVal 
readContents [StringVal name] = liftM StringVal $ liftIO $ readFile name

readAll :: [LispVal] -> ErrorT LispError IO LispVal 
readAll [StringVal name] = liftM ListVal (load name)

load :: String -> ErrorT LispError IO [LispVal]
load name = (liftIO (readFile name)) >>= liftThrows . readExprList

makeFunc varargs env params body = 
    return $ FuncVal (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ErrorT LispError IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ErrorT LispError IO LispVal
makeVarArgs = makeFunc . Just . show

unpackEquals :: LispVal -> LispVal -> Unpacker -> Either LispError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
        unpacked1 <- unpacker arg1
        unpacked2 <- unpacker arg2
        return (unpacked1 == unpacked2)
    `catchError` (const $ return False)


apply :: LispVal -> [LispVal] -> ErrorT LispError IO LispVal
apply (PrimFuncVal func) args = liftThrows (func args)
apply (IOFuncVal func) args = func args
apply (FuncVal params varargs body closure) args =
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) 
                >>= bindVarArgs varargs >>= evalBody
    where 
        remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, ListVal $ remainingArgs)]
            Nothing -> return env


liftThrows :: Either LispError a -> ErrorT LispError IO a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


readExpr :: String -> Either LispError LispVal
readExpr = readOrThrow parseExpr
readExprList :: String -> Either LispError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)


bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where 
        extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do 
        ref <- newIORef value
        return (var, ref)

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= 
    return . maybe False (const True) . lookup var


readOrThrow :: Parser a -> String -> Either LispError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ NoParse err
    Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseAtom 
    <|> parseString 
    <|> try parseFloat 
    <|> parseInt 
    <|> parseQuoted 
    <|> do 
    _ <- char '('
    x <- try parseList <|> parseDottodList
    _ <- char ')'
    return x 


parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"") 
    _ <- char '"'
    return (StringVal x)

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ case atom of 
        "#t" -> BoolVal True
        "#f" -> BoolVal False
        _    -> AtomVal atom

parseInt :: Parser LispVal
parseInt = liftM (IntVal . read) (many1 digit)

parseFloat :: Parser LispVal
parseFloat = do
    str1 <- many1 digit
    dot <- char '.'
    str2 <- many1 digit
    let num = fst . head $ readFloat (str1++dot:str2)
    return (FloatVal num)

parseList :: Parser LispVal
parseList = liftM ListVal $ sepBy parseExpr spaces

parseDottodList :: Parser LispVal
parseDottodList = do
    first <- endBy parseExpr spaces
    rest  <- char '.' >> spaces >> parseExpr
    return (DottedListVal first rest)

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ ListVal [AtomVal "quote", x]

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
