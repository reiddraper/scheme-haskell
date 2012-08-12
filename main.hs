module Main where

import System.Environment
import Control.Monad
import Control.Monad.Error

import Text.ParserCombinators.Parsec hiding (spaces)

------------------------------------------------------------------------------
-- Data declarations
------------------------------------------------------------------------------

-- Langauge data structures
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

-- Errors
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


------------------------------------------------------------------------------
-- Interpreter
------------------------------------------------------------------------------

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
        (throwError $ NotFunction "Unrecognized primitive function args" func)
        ($ args)
        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", oneArgOperator isString),
              ("symbol?", oneArgOperator isSymbol),
              ("number?", oneArgOperator isNumber)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] ->
             ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

-- TODO: better error handling here
oneArgOperator :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
oneArgOperator op []    = return $ Bool False
oneArgOperator op [arg] = return $ op arg
oneArgOperator op _     = return $ Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _          = Bool False

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _          = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _          = Bool False

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                            if null parsed
                                then throwError $ TypeMismatch "number" $ String n
                                else return $  fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

------------------------------------------------------------------------------
-- Error Handling
------------------------------------------------------------------------------

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either LispError

trapError ::  (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

------------------------------------------------------------------------------
-- Printing
------------------------------------------------------------------------------

-- Printing LispVals
instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . "
                                    ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- Printing Errors
instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                               ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                ++ ", found " ++ show found
showError(Parser parseErr)              = "Parse error at " ++ show parseErr


------------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------------

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                    "#t" -> Bool True
                    "#f" -> Bool False
                    otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseListOrDottedList = do char '('
                           x <- (try parseList <|> parseDottedList)
                           char ')'
                           return x

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> parseListOrDottedList

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/<=>?@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

getFirstArg :: IO String
getFirstArg = liftM (!! 0) getArgs

showM :: Either LispError LispVal -> Either LispError String
showM = liftM show

evalString :: String -> Either LispError LispVal
evalString string = readExpr string >>= eval

eitherEvalString :: String -> Either LispError String
eitherEvalString = showM . evalString

eitherEvalStringM :: String -> IO (Either LispError String)
eitherEvalStringM = return . eitherEvalString

showEvaluated :: Either LispError String -> String
showEvaluated = extractValue . trapError

showEvaluatedM :: Either LispError String -> IO String
showEvaluatedM = return . showEvaluated

evalConsoleInput :: IO (Either LispError String)
evalConsoleInput = getFirstArg >>= eitherEvalStringM

showEvaledConsoleInput :: IO String
showEvaledConsoleInput = evalConsoleInput >>= showEvaluatedM

printEvaledConsoleInput :: IO ()
printEvaledConsoleInput = showEvaledConsoleInput >>= putStrLn

main :: IO ()
main = printEvaledConsoleInput
