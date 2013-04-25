
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
-- A Haskell representation and parser for ABC notation. Based on the 2.1 standard. 
-- 
-- For more information see <http://abcnotation.com>.
--
-------------------------------------------------------------------------------------

module Music.Abc (
        Abc(..),     

        AbcHeader(..),
        Information(..),
        Directive(..),

        AbcElement(..),

        -- * Import and export functions
        readAbc,
        showAbc
  ) where



type Abc = [AbcElement]

data AbcHeader = AbcHeader [Information] [Directive]
data Information = Information Char String
data Directive = Directive

data AbcElement 
    = AbcTune ()
    | FreeText String
    | TypesetText ()


readAbc :: String -> Abc
readAbc = error "Not impl"

showAbc :: Abc -> String
showAbc = error "Not impl"