
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
        AbcElement(..),
        Information(..),
        Directive(..),

        -- * Import and export functions
        readAbc,
        showAbc
  ) where



type Abc = [AbcElement]

data AbcElement 
    = AbcTune ()
    | FreeText String
    | TypesetText ()

type Information = ()
type Directive = ()

readAbc :: String -> Abc
readAbc = error "Not impl"

showAbc :: Abc -> String
showAbc = error "Not impl"