module Main where

import System.Environment
import Control.Monad

import Text.ParserCombinators.Parsec hiding (spaces)

------------------------------------------------------------------------------
-- Data declarations
------------------------------------------------------------------------------

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool


------------------------------------------------------------------------------
-- Interpreter
------------------------------------------------------------------------------

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Atom _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

oneArgOperator :: (LispVal -> LispVal) -> [LispVal] -> LispVal
oneArgOperator op []    = Bool False
oneArgOperator op [arg] = op arg
oneArgOperator op _     = Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _          = Bool False

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _          = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _          = Bool False

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
                            if null parsed
                                then 0
                                else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

------------------------------------------------------------------------------
-- Printing
------------------------------------------------------------------------------

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

instance Show LispVal where show = showVal

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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

getFirstArg :: IO String
getFirstArg = liftM (!! 0) getArgs

evalString :: String -> LispVal
evalString = eval . readExpr

evalAndShow :: String -> String
evalAndShow = show . evalString

evalAndPrint :: String -> IO ()
evalAndPrint = putStrLn . evalAndShow

main :: IO ()
main = getFirstArg >>= evalAndPrint
