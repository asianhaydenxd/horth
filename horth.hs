module Horth where

import Data.List (sortBy)
import Data.Function (on)
import Data.Ord (comparing)

data Token
    = ErrorToken String
    | KeyToken String
    | IdToken String
    | SymbolToken String
    | StringToken String
    | CharToken Char
    | IntToken Int
    | FloatToken Float
    deriving (Show, Eq)

-- Constants

letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"
digits = "0123456789"
whitespace = " \t\n"

brackets = [ "{", "}"
           , "[", "]"
           , "(", ")"
           ]

symbols = [ "="
          , ","
          , ";"
          , "|"
          , "->"
          , "=>"
          , "::"
          ]

keywords = [ "if"
           , "else"
           , "let"
           , "peek"
           , "in"
           , "end"
           , "data"
           , "import"
           ]

tokenize :: String -> [Token]
tokenize code = tokenize' 0 []
    where
        tokenize' :: Int -> [Token] -> [Token]
        tokenize' i tokens
            | i >= length code              = tokens
            | (code !! i) `elem` whitespace = tokenize' (i + 1) tokens
            | isBracket i                   = let (j,t) = getBracket i   in tokenize' j (tokens ++ [t])
            | (code !! i) == '"'            = let (j,t) = getString i    in tokenize' j (tokens ++ [t])
            | (code !! i) == '\''           = let (j,t) = getCharacter i in tokenize' j (tokens ++ [t])
            | (code !! i) `elem` letters    = let (j,t) = getWord i      in tokenize' j (tokens ++ [t])
            | (code !! i) `elem` digits     = let (j,t) = getNum i       in tokenize' j (tokens ++ [t])
            | otherwise                     = let (j,t) = getOperator i  in tokenize' j (tokens ++ [t])

        isBracket :: Int -> Bool
        isBracket i = isBracket' i . reverse $ sortBy (comparing length) brackets
            where
                isBracket' :: Int -> [String] -> Bool
                isBracket' _ [] = False
                isBracket' i xs
                    | head xs == take (length $ head xs) (drop i code) = True
                    | otherwise = isBracket' i (tail xs)

        getBracket :: Int -> (Int, Token)
        getBracket i = getBracket' i . reverse $ sortBy (comparing length) brackets
            where
                getBracket' :: Int -> [String] -> (Int, Token)
                getBracket' i xs
                    | head xs == take (length $ head xs) (drop i code) = (i + (length $ head xs), SymbolToken (head xs))
                    | otherwise = getBracket' i (tail xs)
        
        getOperator :: Int -> (Int, Token)
        getOperator i = getOperator' i ""
            where
                getOperator' i name
                    | i >= length code                                     = (i, if name `elem` symbols then SymbolToken name else IdToken name)
                    | (code !! i) `elem` (letters ++ digits ++ whitespace) = (i, if name `elem` symbols then SymbolToken name else IdToken name)
                    | otherwise                                            = getOperator' (i + 1) (name ++ [code !! i])

        getWord :: Int -> (Int, Token)
        getWord i = getWord' i ""
            where
                getWord' :: Int -> String -> (Int, Token)
                getWord' i name
                    | i >= length code                       = (i, if name `elem` keywords then KeyToken name else IdToken name)
                    | (code !! i) `elem` (letters ++ digits) = getWord' (i + 1) (name ++ [code !! i])
                    | otherwise                              = (i, if name `elem` keywords then KeyToken name else IdToken name)

        getNum :: Int -> (Int, Token)
        getNum i = getNum' i ""
            where
                getNum' :: Int -> String -> (Int, Token)
                getNum' i name
                    | (length . filter ('.' ==)) name > 1 = (i, ErrorToken "too many dots")
                    | i >= length code                    = (i, if '.' `elem` name then FloatToken (read name) else IntToken (read name))
                    | (code !! i) `elem` (digits ++ ".")  = getNum' (i + 1) (name ++ [code !! i])
                    | otherwise                           = (i, if '.' `elem` name then FloatToken (read name) else IntToken (read name))
        
        getString :: Int -> (Int, Token)
        getString i = getString' (i + 1) ""
            where
                getString' :: Int -> String -> (Int, Token)
                getString' i name
                    | i >= length code   = (i + 1, ErrorToken "string never closed")
                    | (code !! i) /= '"' = getString' (i + 1) (name ++ [code !! i])
                    | otherwise          = (i + 1, StringToken name)
        
        getCharacter :: Int -> (Int, Token)
        getCharacter i = getCharacter' (i + 1) ""
            where
                getCharacter' :: Int -> String -> (Int, Token)
                getCharacter' i name
                    | i >= length code    = (i + 1, ErrorToken "character never closed")
                    | (code !! i) /= '\'' = getCharacter' (i + 1) (name ++ [code !! i])
                    | otherwise           = (i + 1, if length name /= 1 then ErrorToken "character only has one character" else CharToken $ head name)

data Globe
    = Globe [Node]
    deriving Show

data Node
    = Declaration String Type
    | ProcBlock String [Proc]
    deriving Show

data Type
    = ProcType Type Type
    | IntType Int
    | FloatType Float
    | BoolType Bool
    | CharType Char
    | ListType [Type]
    | PairType (Type, Type)
    deriving Show

data Proc
    = Proc String
    | LetBind [String]
    deriving Show

parse :: [Token] -> Globe
parse = undefined

main :: IO ()
main = do
    code <- readFile "main.horth"
    -- print $ tokenize code
    print $ Globe [ ProcBlock "isEven" [Proc "puts"] ]