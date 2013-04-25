
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
        AbcFile(..),     

        FileHeader(..),
        Information(..),
        Directive(..),

        Element(..),

        -- * Import and export functions
        readAbc,
        showAbc
  ) where



data AbcFile = AbcFile FileHeader [Element]

data FileHeader = FileHeader [Information] [Directive]
    deriving (Eq, Ord, Show)

data Information = Information Char String
    deriving (Eq, Ord, Show)

data Directive = Directive
    deriving (Eq, Ord, Show)


data Element 
    = AbcTune ()
    | FreeText String
    | TypesetText String
    deriving (Eq, Ord, Show)




-- Base types

newtype Duration = Duration { getDuration :: Rational }

data PitchClass = C | D | E | F | G | A | B
    deriving (Eq, Ord, Show, Enum)

data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
    deriving (Eq, Ord, Show, Enum)

newtype Octave = Octave { getOctave :: Int }
    deriving (Eq, Ord, Show, Enum)

newtype Pitch = Pitch { getPitch :: (PitchClass, Accidental, Octave) }
    deriving (Eq, Ord, Show)

data Barline
    = Barline
    | DoubleBarline Bool Bool   -- thick? thick?
    | Repeat Int Bool Bool      -- times end? begin?
    | DottedBarline Barline
    | InvisibleBarline Barline


    
    

readAbc :: String -> AbcFile
readAbc = error "Not impl"

showAbc :: AbcFile -> String
showAbc = error "Not impl"