module Tree
    ( Tree (..)
    , parse
    ) where

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Char
import Text.Printf
import System.FilePath

data Tree = Root [Tree]
          | Dir String FilePath [Tree]
          | File String FilePath

instance Show Tree where
    show = go ""
        where go _ (Root children) = mconcat $ "/\n" : map (go "/") children
              go path (Dir name _ children) = mconcat $ printf "%s/\n" (path </> name) : map (go (path </> name)) children
              go path (File name _) = printf "%s\n" $ path </> name

type Parser = StateT String Maybe

runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT

charP :: (Char -> Bool) -> Parser Char
charP pred = do
    s <- get
    case s of
        "" -> mzero
        (x:xs)
            | pred x -> put xs >> pure x
            | otherwise -> mzero

char :: Char -> Parser Char
char = charP . (==)

ws :: Parser ()
ws = void $ charP isSpace

string :: String -> Parser String
string = mapM char

root :: Parser ()
root = void $ some ws

parseRoot :: Parser Tree
parseRoot = do
    root
    Root <$> many (parseTree "/")

dir :: Parser String
dir = do
    name <- some $ charP (/='/')
    char '/'
    some ws
    pure name

parseDir :: FilePath -> Parser Tree
parseDir prefix = do
    name <- dir
    Dir name "" <$> many (parseTree $ prefix </> name ++ "/")

file :: Parser String
file = do
    name <- some $ charP $ not . isSpace
    some ws
    pure name

parseFile :: Parser Tree
parseFile = do
    name <- file
    pure $ File name ""

parseTree :: FilePath -> Parser Tree
parseTree prefix = do
    string prefix
    parseRoot <|> parseDir prefix <|> parseFile

parse :: String -> Maybe Tree
parse s = runParser (parseTree "/") s >>= \(tree, rest) -> guard (null rest) >> pure tree
