{-# LANGUAGE ExistentialQuantification #-}

module Main where

import System.Environment
import System.Exit
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
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         Bool False -> eval alt
         otherwise -> eval conseq
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
              ("number?", oneArgOperator isNumber),

              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),

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
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] ->
             ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) ->
          [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left  <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop    = boolBinop unpackNum
strBoolBinop    = boolBinop unpackStr
boolBoolBinop   = boolBinop unpackBool

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

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                            if null parsed
                                then throwError $ TypeMismatch "number" $ String n
                                else return $  fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- List Funcs

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]      = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]  = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]      = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)]  = eqv [List $ xs ++ [x],
                                                   List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2)
                               && (all eqvPair $ zip arg1 arg2)
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

eqvPair ::  (LispVal, LispVal) -> Bool
eqvPair (x1, x2) = case eqv [x1, x2] of
                     Left error -> False
                     Right (Bool val) -> val

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

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

showEvaluatedString :: String -> String
showEvaluatedString = showEvaluated . eitherEvalString

showEvaluatedStringM :: (Monad m) => String -> m String
showEvaluatedStringM = return . showEvaluatedString

showEvaledFileFromName :: FilePath -> IO String
showEvaledFileFromName fileName = (readFile fileName) >>= showEvaluatedStringM

printExpression :: String -> IO ()
printExpression = putStrLn . showEvaluatedString

printFileEval :: FilePath -> IO ()
printFileEval fileName =  (showEvaledFileFromName fileName) >>= putStrLn

processArgs :: [String] -> IO ()
processArgs [] = putStrLn "this will go into the REPL"
processArgs ["-e", expr] = printExpression expr
processArgs [fileName] = printFileEval fileName

main :: IO ()
main = getArgs >>= processArgs
