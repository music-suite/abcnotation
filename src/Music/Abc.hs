
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

-- TODO compare https://github.com/sergi/abcnode/blob/master/parser.pegjs

module Music.Abc (

        ----------------------------------------------------------------------

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

        ----------------------------------------------------------------------

        -- * Music
        Music(..),
        Chord(..),
        Barline(..),
        Annotation(..),
        ChordSymbol(..),
        Decoration(..),
        Dynamic(..),

        -- ** Time
        Duration(..),
        Meter(..),
        Tempo(..),

        -- ** Pitch
        PitchClass(..),
        Accidental(..),
        Octave(..),
        Pitch(..),
        Key(..),
        StemDirection(..),
        Clef(..),
        Mode(..),

        ----------------------------------------------------------------------

        -- * Information
        Information(..),
        Directive(..),
        VoiceProperties(..),

        ----------------------------------------------------------------------

        -- * Import and export
        readAbc,
        showAbc
  ) where


-- | A full ABC file (2.2).
data AbcFile
    = AbcFile
        (Maybe String)
        (Maybe FileHeader)
        [Element]
    deriving (Eq, Ord, Show)

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

data TuneHeader
    = TuneHeader
        [Information]
    deriving (Eq, Ord, Show)

-- | One line of music code.
type TuneBody 
    = [Music]


--------------------------------------------------------------------------------

-- | One line of music code.
data Music
    = Chord Chord
    | Barline Barline
    | Tie Music
    | Slur Music
    | Beam Music
    | Grace Music
    | Tuplet Duration Music
    | Decorate Decoration Music
    | Annotate Annotation Music
    | ChordSymbol ChordSymbol Music
    | Sequence [Music] -- beam? music
    deriving (Eq, Ord, Show)

data Annotation
    = AnnotateLeft String
    | AnnotateRight String
    | AnnotateAbove String
    | AnnotateBelow String
    | AnnotateUnspecified String
    deriving (Eq, Ord, Show)
    
-- TODO clefs and transposition (4.6)
-- TODO redifinable symbols (4.16)
-- TODO symbol lines (4.15)
-- TODO symbol lyrics

-- Note (4.20) 
type Chord = (
        [Pitch],
        (Maybe Duration)
    )

type ChordSymbol 
    = String

-- | Barline, including special barlines and repeats.
data Barline
    = SingleBarline
    | DoubleBarline Bool Bool           -- thick? thick?
    | Repeat Int Bool Bool              -- times end? begin?
    | DottedBarline Barline
    | InvisibleBarline Barline
    deriving (Eq, Ord, Show)

-- TODO first and second repeats (4.9)
-- TODO variant endings (4.10)


-- | Decorations (4.14)
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
    | Dynamic Dynamic         -- Dynamics
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


--------------------------------------------------------------------------------

-- Base types

-- | Accidentals (4.2).
data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Pitch class (4.1).
data PitchClass = C | D | E | F | G | A | B
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Octaves (4.1).
newtype Octave = Octave { getOctave :: Int }
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

-- | Pitch (4.1, 4.2).
newtype Pitch = Pitch { getPitch :: (PitchClass, Accidental, Octave) }
    deriving (Eq, Ord, Show)


data StemDirection = Up | Down
    deriving (Eq, Ord, Show, Enum, Bounded)

data Clef = NoClef | Treble | Alto | Tenor | Bass | Perc
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Duration (4.3).
newtype Duration = Duration { getDuration :: Rational }
    deriving (Eq, Ord, Show, Enum, Num, Real, Fractional, RealFrac)

data Information
    = Area String
    | Book String
    | Composer String
    | Discography String
    | FileUrl String
    | Group String
    | History String
    | Instruction Directive
    | Key Key
    | UnitNoteLength Duration
    | Meter Meter            
    | Macro                             -- ^ Macro (not supported)
    | Notes String                      -- ^ Notes
    | Origin String                     -- ^ Origin of tune.
    | Parts
    | Tempo Tempo                       -- ^ Tempo of tune.
    | Rhythm String                     -- ^ Rhythm type of tune.
    | Remark                            -- ^ Remarks (not supported)
    | Source String                     -- ^ Source material.
    | SymbolLine
    | Title String                      -- ^ Title of tune.
    | UserDefined                       -- ^ User defined (not supported)
    | Voice VoiceProperties
    | Words String
    | ReferenceNumber Integer
    | Transcription String
    deriving (Eq, Ord, Show)

type Key = (PitchClass, Mode)

-- | Optional string, numerators, frequency (3.1.8)
type Tempo = (Maybe String, [Duration], Duration)

data VoiceProperties
    = VoiceProperties
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
    (Just $ FileHeader [] []) 
    [
        Tune (AbcTune 
            (TuneHeader [
                ReferenceNumber     19004,
                Title               "Silent Night",
                Title               "Stille Nacht! Heilige Nacht!",
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
            ]) 
            [])
    ]






