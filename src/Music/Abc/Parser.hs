
-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------------------------------

module Music.Abc.Parser (
    parse
  ) where

import Data.Monoid
import Control.Monad
import Control.Applicative hiding ((<|>), optional, many)

import Text.Parsec hiding (parse)
import Text.Parsec.Token
import Text.Parsec.String

import Music.Abc

-- TODO information field verification (header/body)

-- |
-- Parse a module description, returning an error if unsuccessful.
--
parse :: String -> Either ParseError Abc
parse = runParser abcFile () ""
                 
-------------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------------

abcFile :: Parser Abc
abcFile = do                      
    -- optional byteOrderMark
    string "%abc"
    optional $ string "-" >> version
    optional $ fileHeader    
    fileBody
    return undefined

fileHeader :: Parser [Information]
fileHeader = many1 $ mzero 
    <|> informationField 
    <|> styleSheetDirective

fileBody :: Parser [AbcElement]
fileBody = (flip sepBy) emptyLine $ mzero
    <|> fmap AbcTune abcTune
    <|> fmap FreeText freeText 
    <|> fmap TypesetText typeSetText

informationField :: Parser Information
informationField = do
    letter
    char ':'
    -- TODO anything not \n
    char '\n'              
    return undefined

styleSheetDirective :: Parser Directive
styleSheetDirective =
    return undefined


abcTune :: Parser ()
abcTune = undefined

freeText :: Parser String
freeText = undefined

typeSetText :: Parser ()
typeSetText = undefined



byteOrderMark :: Parser ()
byteOrderMark = do
    char '\xFFFE' <|> char '\xFEFF'
    return ()

version :: Parser Double
version = undefined

-------------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------------

lexer :: TokenParser ()
lexer = makeTokenParser $ LanguageDef {
    commentStart    =  "[r:",
    commentEnd      =  "]",
    commentLine     =  "%",
    nestedComments  =  False,
    identStart      =  (letter <|> char '_'),
    identLetter     =  (alphaNum <|> char '_'),
    opStart         =  mzero,
    opLetter        =  mzero,
    reservedNames   =  reservedNames,
    reservedOpNames =  mzero,
    caseSensitive   =  True
    }
    where
        reservedNames = []

-- Convenient synonyms, not exported
llex   = lexeme lexer
lnat   = natural lexer
lstr   = stringLiteral lexer
lname  = identifier lexer
lres   = reserved lexer
lspace = whiteSpace lexer

emptyLine = newLine >> newLine
newLine = string "\r\n" <|> string "\n"

single x = [x]

notSupported x = error $ "Not supported yet: " ++ x
                                                         