
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
        -- * Abc format
        -- ** Files
        AbcFile(..),     

        -- *** File header
        FileHeader(..),
        Element(..),

        -- ** Tunes
        AbcTune(..),
        TuneHeader(..), 
        TuneBody(..), 

        -- * Music
        Music(..),
        
        -- ** Note stack
        Note(..),
        GraceT(..),
        BeamT(..),
        SlurT(..),
        DecorationT(..),
        TupletT(..),
        DurationT(..),
        RestT(..),
        
        -- * Basic types
        -- ** Time
        Duration(..),

        -- ** Pitch
        PitchClass(..),
        Accidental(..),
        Octave(..),
        Pitch(..),

        -- ** Decorations (articulation, dynamics etc)
        Decoration(..),

        -- ** Structure
        Barline(..),

        -- ** Information etc
        Information(..),
        Directive(..),

        -- * Import and export
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
    = Tune AbcTune
    | FreeText String
    | TypesetText String
    deriving (Eq, Ord, Show)

data AbcTune = AbcTune TuneHeader TuneBody
    deriving (Eq, Ord, Show)

-- TODO verify X, T and K fields
data TuneHeader = TuneHeader [Information]
    deriving (Eq, Ord, Show)

type TuneBody = [Music]

-- One line of music code.
data Music 
    = Music [Either Note Barline]
    deriving (Eq, Ord, Show)

type Note = GraceT (SlurT (DecorationT (BeamT (TupletT (DurationT (RestT Pitch))))))

type GraceT a       = (Bool, a)
type BeamT a        = (Bool, a, Bool)
type SlurT a        = (Bool, a, Bool)
type DecorationT a  = ([Decoration], a)
type TupletT a      = (Duration, a)
type DurationT a    = (a, Duration)
type RestT          = Maybe

data Decoration
    = Trill                   --                "tr" (trill mark)
    | TrillBegin                   --               start of an extended trill
    | TrillEnd                   --               end of an extended trill
    | Lowermordent            --         short /|/|/ squiggle with a vertical line through it
    | Uppermordent            --         short /|/|/ squiggle
    | Mordent                 --              same as !lowermordent!
    | Pralltriller            --         same as !uppermordent!
    | Roll                    --                 a roll mark (arc) as used in Irish music
    | Turn                    --                 a turn mark (also known as gruppetto)
    | Turnx                   --                a turn mark with a line through it
    | Invertedturn            --         an inverted turn mark
    | Invertedturnx           --        an inverted turn mark with a line through it
    | Arpeggio                --             vertical squiggle
    | Mark                    --                    > mark
    | Accent                  --               same as !>!
    | Emphasis                --             same as !>!
    | Fermata                 --              fermata or hold (arc above dot)
    | Invertedfermata         --      upside down fermata
    | Tenuto                  --               horizontal line to indicate holding note for full duration
    | Fingering Int           -- fingerings
    | Plus                    -- left-hand pizzicato, or rasp for French horns
    | Snap                    --                 snap-pizzicato mark, visually similar to !thumb!
    | Slide                   --                slide up to a note, visually similar to a half slur
    | Wedge                   --                small filled-in wedge mark
    | Upbow                   --                V mark
    | Downbow                 --              squared n mark
    | Open                    --                 small circle above note indicating open string or harmonic
    | Thumb                   --                cello thumb symbol
    | Breath                  --               a breath mark (apostrophe-like) after note
    -- !pppp! !ppp! !pp! !p!  dynamics marks
    -- !mp! !mf! !f! !ff!     more dynamics marks
    -- !fff! !ffff! !sfz!     more dynamics marks
    | Crescendo               --           start of a < crescendo mark
    | EndCrescendo            --           end of a < crescendo mark, placed after the last note
    | Diminuendo              --          start of a > diminuendo mark
    | EndDiminuendo           --          end of a > diminuendo mark, placed after the last note
    | Segno                   --                2 ornate s-like symbols separated by a diagonal line
    | Coda                    --                 a ring with a cross in it
    | DaSegno                 -- the letters D.S. (=Da Segno)
    | DaCapo                  -- the letters D.C. (=either Da Coda or Da Capo)
    | Dacoda                  --               the word "Da" followed by a Coda sign
    | Fine                    --                 the word "fine"
    | Shortphrase             --          vertical line on the upper part of the staff
    | Mediumphrase            --         same, but extending down to the centre line
    | Longphrase              --           same, but extending 3/4 of the way down
    deriving (Eq, Ord, Show)


-- Base types

newtype Duration = Duration { getDuration :: Rational }
    deriving (Eq, Ord, Show, Enum, Num, Fractional, Real, RealFrac)

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
    deriving (Eq, Ord, Show)


    
    

readAbc :: String -> AbcFile
readAbc = error "Not impl"

showAbc :: AbcFile -> String
showAbc = error "Not impl"