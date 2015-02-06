{-# OPTIONS_GHC -Wall #-}

module Val (
    LispVal (..), 
    LispError (..), 
    Env, 
    unwordsList
) where 

import Data.IORef (IORef)
import Text.ParserCombinators.Parsec (ParseError)
-- import System.Environment (getArgs)
import System.IO (Handle)
import Control.Monad.Error (Error (..), ErrorT)


type Env = IORef [(String, IORef LispVal)]
-- type ThrowsError = Either LispError
-- type IOThrowsError = ErrorT LispError IO

data LispVal = AtomVal String
             | ListVal [LispVal]
             | DottedListVal [LispVal] LispVal
             | IntVal Integer
             | FloatVal Double
             | StringVal String
             | BoolVal Bool
             | PrimFuncVal ([LispVal] -> Either LispError LispVal)
             | FuncVal { params :: [String], vararg :: (Maybe String),
                         body :: [LispVal], closure :: Env }
             | IOFuncVal ([LispVal] -> ErrorT LispError IO LispVal)
             | PortVal Handle
--              deriving (Show)

instance Show LispVal where 
--     show :: LispVal -> String
    show (StringVal str) = "\"" ++ str ++ "\""
    show (AtomVal name)  = name
    show (IntVal num)    = show num
    show (FloatVal num)  = show num
    show (BoolVal True)  = "#t"
    show (BoolVal False) = "#f"
    show (ListVal contents)         = "(" ++ unwordsList contents ++ ")"
    show (DottedListVal first rest) = "(" ++ unwordsList first ++ 
                                    " . " ++ show rest ++ ")"
    show (PrimFuncVal _) = "<primitive>"
    show (FuncVal {params=args, vararg=varargs, body=_, closure=_}) = 
        "(lambda (" ++ unwords (map show args) ++
            (case varargs of
                Nothing -> ""
                Just arg -> " . " ++ arg) ++ ") ...)"
    show (IOFuncVal _) = "<IO primitive>"
    show (PortVal _) = "<IO port>"

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | NoParse ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    --     show :: LispError -> String
    show (UnboundVar message varname)  = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func)    = message ++ ": " ++ show func
    show (NumArgs expected found)      = "Expected " ++ show expected 
                                         ++ " args; found values " 
                                         ++ unwordsList found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                         ++ ", found " ++ show found
    show (NoParse parseErr)            = "Parse error at " ++ show parseErr
    show (Default str)                 = "default error: " ++ show str

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
