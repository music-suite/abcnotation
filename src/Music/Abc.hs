
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

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

        DecorationT(..),
        SlurT(..),
        BeamT(..),
        GraceT(..),
        TupletT(..),
        DurationT(..),
        RestT(..),
        (:|:),
        
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
        MultiRest(..),

        -- ** Information etc
        Information(..),
        Directive(..),

        -- * Import and export
        readAbc,
        showAbc
  ) where

import Network.URI (URI)

-- | A full ABC file (2.2).
data AbcFile
    = AbcFile
        (Maybe String)
        (Maybe FileHeader)
        [Element]

-- | File header (2.2.2).
data FileHeader
    = FileHeader 
        [Information] 
        [Directive]
    deriving (Eq, Ord, Show)

-- | Either a tune, free text or typeset text (2.2.3).
data Element
    = Tune
        AbcTune                         -- ^ An Abc tune.
    | FreeText
        String                          -- ^ Free text (2.2.3).
    | TypesetText
        String                          -- ^ Typeset text (2.2.3).
    deriving (Eq, Ord, Show)

data AbcTune 
    = AbcTune 
        TuneHeader 
        TuneBody
    deriving (Eq, Ord, Show)

-- TODO verify X, T and K fields
data TuneHeader 
    = TuneHeader 
        [Information]
    deriving (Eq, Ord, Show)

type TuneBody = [Music]

-- One line of music code.
data Music 
    = Music [Note :|: MultiRest :|: Barline :|: ()]
    deriving (Eq, Ord, Show)

newtype MultiRest = MultiRest { getMultiRest :: Int }
    deriving (Eq, Ord, Show)

type Note = DecorationT (SlurT (BeamT (GraceT (TupletT (DurationT (RestT Pitch))))))

type DecorationT a  = ([Decoration], a)
type SlurT a        = (Bool, a, Bool)
type BeamT a        = (Bool, a, Bool)
type GraceT a       = (Bool, a)
type TupletT a      = (Duration, a)
type DurationT a    = (a, Duration)
type RestT a        = Maybe (Maybe a) -- invisible/visible

data Decoration
    = Trill                   -- "tr" (trill mark)
    | TrillBegin              -- start of an extended trill
    | TrillEnd                -- end of an extended trill
    | Lowermordent            -- short squiggle with a vertical line through it
    | Uppermordent            -- short squiggle
    | Roll                    -- a roll mark (arc) as used in Irish music
    | Turn                    -- a turn mark (also known as gruppetto)
    | Turnx                   -- a turn mark with a line through it
    | Invertedturn            -- an inverted turn mark
    | Invertedturnx           -- an inverted turn mark with a line through it
    | Arpeggio                -- vertical squiggle
    | Accent                  -- accent mark
    | Fermata Bool            -- fermata or hold (arc above dot), inverted?
    | Tenuto                  -- horizontal line to indicate holding note for full duration
    | Fingering Int           -- fingerings
    | Plus                    -- left-hand pizzicato, or rasp for French horns
    | Snap                    -- snap-pizzicato mark, visually similar to !thumb!
    | Slide                   -- slide up to a note, visually similar to a half slur
    | Wedge                   -- small filled-in wedge mark
    | Upbow                   -- V mark
    | Downbow                 -- squared n mark
    | Open                    -- small circle above note indicating open string or harmonic
    | Thumb                   -- cello thumb symbol
    | Breath                  -- a breath mark (apostrophe-like) after note
    -- !pppp! !ppp! !pp! !p!  dynamics marks
    -- !mp! !mf! !f! !ff!     more dynamics marks
    -- !fff! !ffff! !sfz!     more dynamics marks
    | Crescendo               -- start of a crescendo mark
    | EndCrescendo            -- end of a crescendo mark, placed after the last note
    | Diminuendo              -- start of a diminuendo mark
    | EndDiminuendo           -- end of a diminuendo mark, placed after the last note
    | Segno                   -- ornate s-like symbols separated by a diagonal line
    | Coda                    -- a ring with a cross in it
    | DaSegno                 -- the letters D.S. (=Da Segno)
    | DaCapo                  -- the letters D.C. (=either Da Coda or Da Capo)
    | Dacoda                  -- the word "Da" followed by a Coda sign
    | Fine                    -- the word "fine"
    | Shortphrase             -- vertical line on the upper part of the staff
    | Mediumphrase            -- same, but extending down to the centre line
    | Longphrase              -- same, but extending 3/4 of the way down
    deriving (Eq, Ord, Show)


-- Base types

newtype Duration = Duration { getDuration :: Rational }
    deriving (Eq, Ord, Show, Enum, Num, Real, Fractional, RealFrac)

data PitchClass = C | D | E | F | G | A | B
    deriving (Eq, Ord, Show, Enum, Bounded)

data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
    deriving (Eq, Ord, Show, Enum, Bounded)

newtype Octave = Octave { getOctave :: Int }
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

newtype Pitch = Pitch { getPitch :: (PitchClass, Accidental, Octave) }
    deriving (Eq, Ord, Show)

data StemDirection = Up | Down
    deriving (Eq, Ord, Show, Enum, Bounded)

data Clef = NoClef | Treble | Alto | Tenor | Bass | Perc
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Barline, including special barlines and repeats.
data Barline
    = Barline
    | DoubleBarline Bool Bool   -- thick? thick?
    | Repeat Int Bool Bool      -- times end? begin?
    | DottedBarline Barline
    | InvisibleBarline Barline
    deriving (Eq, Ord, Show)

-- TODO add elements
data Information
    = Area String
    | Book String
    | Composer String
    | Discography String
    | FileUrl String URI
    | Group String
    | History String

    | Instruction Directive
    | Key Key
    | UnitNoteLength Duration

    | Meter Meter
    -- Macros not supported
    | Notes String
    | Origin String
    | Parts -- TODO

    | Tempo Tempo
    | Rhythm String -- Polska, marsch etc.
    -- Remarks are discarded
    | Source String -- Uppland etc.

    | SymbolLine Symbol 
    | TuneTitle String
    -- User defined not supported

    | Voice VoiceProps
    | Words String -- TODO include separators
    | ReferenceNumber Integer
    | Transcription String
    deriving (Eq, Ord, Show)

type Key = (PitchClass, Mode)                   

-- | Optional string, beats, frequency (3.1.8)
type Tempo = (Maybe String, [Duration], Duration)

type Symbol = ()

data VoiceProps
    = VoiceProps
        (Maybe String)
        (Maybe String)
        (Maybe StemDirection)
        (Maybe Clef)
    deriving (Eq, Ord, Show)

data Meter
    = NoMeter
    | Common
    | Cut
    | Simple Rational
    | Compound [Integer] Integer
    deriving (Eq, Ord, Show)

data Mode
    = Major
    | Minor
    | Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Locrian
    deriving (Eq, Ord, Show)

-- | Abc directive.
type Directive = (String, String)
    
    

readAbc :: String -> AbcFile
readAbc = error "Not impl"

showAbc :: AbcFile -> String
showAbc = error "Not impl"

infixr 5 :|:
type a :|: b = Either a b

