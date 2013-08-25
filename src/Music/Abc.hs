
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
        noVersion,
        noHeader,
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
        DurationT(..),
        RestT(..),
        (:|:),


        -- * Basic types
        -- ** Time
        Duration(..),
        Meter(..),
        Tempo(..),
        VoiceProps(..),

        -- ** Pitch
        PitchClass(..),
        Accidental(..),
        Octave(..),
        Pitch(..),
        Key(..),
        StemDirection(..),
        Clef(..),
        Mode(..),

        -- ** Symbols
        Symbol(..),

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
    deriving (Eq, Ord, Show)


noVersion = Nothing
noHeader  = Nothing

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

-- | One line of music code.
type TuneBody = [Music]







-- | One line of music code.
data Music
    = Music [Note :|: MultiRest :|: Barline :|: ()]
    deriving (Eq, Ord, Show)

-- TODO broken rhythm (4.4)

-- Note (4.20) grace chordSym ann/dec acci note/rest octave dur
type Note = DecorationT (SlurT (BeamT (GraceT (DurationT (RestT Pitch)))))

-- | Rests (4.5)
type RestT a        = Maybe (Maybe a)       -- invisible/visible

-- TODO clefs and transposition (4.6)

-- | Beams (4.7)
type BeamT a        = (Bool, a, Bool)

-- | Slurs (4.11)
type SlurT a        = (Bool, a, Bool)
-- TODO ties (4.11)

-- | Grace notes (4.12)
type GraceT a       = (Bool, a)

-- TODO tuplets (4.13)

-- | Decorations (4.14)
type DecorationT a  = ([Decoration], a)
type DurationT a    = (a, Duration)

-- TODO symbol lines (4.15)
-- TODO redifinable symbols (4.16)

-- TODO chords (4.17)
-- TODO chord symbols (4.18)
-- TODO annotations (4.19)


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
    | Dynamic Dynamic         -- Dynamics
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

data Dynamic
    = PPPP
    | PPP
    | PP
    | P_
    | MP
    | MF
    | F_
    | FF
    | FFF
    | FFFF
    | SFZ
    deriving (Eq, Ord, Show)

-- Rests

newtype MultiRest = MultiRest { getMultiRest :: Int }
    deriving (Eq, Ord, Show)

-- | Barline, including special barlines and repeats.
data Barline
    = Barline
    | DoubleBarline Bool Bool   -- thick? thick?
    | Repeat Int Bool Bool      -- times end? begin?
    | DottedBarline Barline
    | InvisibleBarline Barline
    deriving (Eq, Ord, Show)

-- TODO first and second repeats (4.9)
-- TODO variant endings (4.10)






-- Base types

-- | Duration (4.3).
newtype Duration = Duration { getDuration :: Rational }
    deriving (Eq, Ord, Show, Enum, Num, Real, Fractional, RealFrac)

-- | Accidentals (4.2).
data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Pitch class (4.1).
data PitchClass = C | D | E | F | G | A | B
    deriving (Eq, Ord, Show, Enum, Bounded)

newtype Octave = Octave { getOctave :: Int }
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

newtype Pitch = Pitch { getPitch :: (PitchClass, Accidental, Octave) }
    deriving (Eq, Ord, Show)

data StemDirection = Up | Down
    deriving (Eq, Ord, Show, Enum, Bounded)

data Clef = NoClef | Treble | Alto | Tenor | Bass | Perc
    deriving (Eq, Ord, Show, Enum, Bounded)

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

-- | Optional string, numerators, frequency (3.1.8)
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


{-
    
    X:19004
    T:Silent Night
    T:Stille Nacht! Heilige Nacht!
    R:Air
    C:Franz Xaver Gruber, 1818
    O:Austria
    Z:Paul Hardy's Xmas Tunebook 2012 (see www.paulhardy.net). Creative Commons cc by-nc-sa licenced.
    M:6/8
    L:1/8
    Q:3/8=60
    K:C
    "C"G>A G E3|G>A G E2z|"G"d2 d B3|"C"c2 c G2z|
    "F"A2 A c>B A|"C"G>A G E2z|"F"A2 A c>B A|"C"G>A G E2z|
    "G7"d2 d f>d B|"C"c3 e2z|cGE "G7"G>F D|"C"C3-C3|]
    W:Silent night, holy night
    W:All is calm, all is bright
    W:Round yon Virgin Mother and Child
    W:Holy Infant so tender and mild
    W:Sleep in heavenly peace
    W:Sleep in heavenly peace
    W:
    W:(Josef Mohr, 1818, Trans by John Young, 1819)
    
-}
test = AbcFile 
    (Just "1.2") 
    (Just $ FileHeader [ 
            ReferenceNumber     19004,
            TuneTitle           "Silent Night",
            TuneTitle           "Stille Nacht! Heilige Nacht!",
            Rhythm              "Air",
            Composer            "Franz Xaver Gruber, 1818",
            Origin              "Austria",
            Source              "Paul Hardy's Xmas Tunebook 2012",
            Meter               (Simple $ 6/8),
            UnitNoteLength      (1/8),
            Tempo               (Nothing, [3/8], 60),
            Key                 (C, Major),            

            Words               "Silent night, holy night",
            Words               "All is calm, all is bright",
            Words               "Round yon Virgin Mother and Child",
            Words               "Holy Infant so tender and mild",
            Words               "Sleep in heavenly peace",
            Words               "Sleep in heavenly peace"
        ] []) 
    [
    
    ]





infixr 5 :|:
type a :|: b = Either a b

