
{-# LANGUAGE TypeOperators, OverloadedStrings, GeneralizedNewtypeDeriving #-}

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

module Data.Music.Abc (

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

import Data.Maybe
import Data.Ratio
import Data.Char
import Data.Semigroup
import Data.List (intersperse)
import Text.Pretty hiding (Mode)

--------------------------------------------------------------------------------

-- File structure


-- | A full ABC file (2.2).
data AbcFile
    = AbcFile
        (Maybe String)
        (Maybe FileHeader)
        [Element]
    deriving (Eq, Ord, Show)

instance Pretty AbcFile where
    pretty (AbcFile version header elements) = mempty
        <> "%abc-" <> string (fromMaybe "2.1" version) <> "\n"
        <> pretty header <> "\n"
        <> sepBy "\n" (fmap pretty elements) <> "\n"
    
    
-- | File header (2.2.2).
data FileHeader
    = FileHeader
        [Information]
        [Directive]
    deriving (Eq, Ord, Show)

instance Pretty FileHeader where
    pretty (FileHeader info directives) = mempty
        <> sepBy "\n" (fmap pretty info) <> "\n"
        <> sepBy "\n" (fmap pretty directives)


-- | Either a tune, free text or typeset text (2.2.3).
data Element
    = Tune
        AbcTune                         -- ^ An Abc tune.
    | FreeText
        String                          -- ^ Free text (2.2.3).
    | TypesetText
        String                          -- ^ Typeset text (2.2.3).
    deriving (Eq, Ord, Show)

instance Pretty Element where
    pretty (Tune a)         = pretty a
    pretty (FreeText a)     = string a
    pretty (TypesetText a)  = string a


data AbcTune
    = AbcTune
        TuneHeader
        TuneBody
    deriving (Eq, Ord, Show)

instance Pretty AbcTune where
    pretty (AbcTune header elements) = mempty
        <> pretty header <> "\n"
        <> sepBy "\n" (fmap pretty elements) <> "\n"


data TuneHeader
    = TuneHeader
        [Information]
    deriving (Eq, Ord, Show)

instance Pretty TuneHeader where
    pretty (TuneHeader info) =
        sepBy "\n" (fmap pretty info)


-- | Lines of music code.
type TuneBody 
    = [Music]
-- TODO voices, see http://www.barfly.dial.pipex.com/multivoice.txt


--------------------------------------------------------------------------------

-- Music


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

instance Pretty Music where
    pretty = go
        where
            go (Chord a) = pretty a
            go (Sequence as) = sepBy " " $ fmap pretty as
    -- FIXME


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
newtype Chord = Chord_ { getChord :: (
        [Pitch],
        (Maybe Duration)
    ) }
    deriving (Eq, Ord, Show)

instance Pretty Chord where
    -- TODO skip duration if zero
    pretty (Chord_ ([], dur))       = ""
    pretty (Chord_ ([pitch], dur))  =
        pretty pitch <> pretty dur
    pretty (Chord_ (pitches, dur))  =
        brackets (sepBy "" (fmap pretty pitches)) <> pretty dur


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

-- Information


-- | An information field (3).
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

instance Pretty Information where
    pretty a = string $ fieldName a ++ ": " ++ showField a


fieldName :: Information -> String
fieldName = go
    where
        go (Area _)                 = "A"
        go (Book _)                 = "B"
        go (Composer _)             = "C"
        go (Discography _)          = "D"
        go (FileUrl _)              = "F"
        go (Group _)                = "G"
        go (History _)              = "H"
        go (Instruction _)          = "I"
        go (Key _)                  = "K"
        go (UnitNoteLength _)       = "L"
        go (Meter _)                = "M"
        go Macro                    = "m"
        go (Notes _)                = "N"
        go (Origin _)               = "O"
        go Parts                    = "O"
        go (Tempo _)                = "Q"
        go (Rhythm _)               = "R"
        go Remark                   = "r"
        go (Source _)               = "S"
        go SymbolLine               = "s"
        go (Title _)                = "T"
        go UserDefined              = "U"
        go (Voice _)                = "V"
        go (Words _)                = "W"
        go (ReferenceNumber _)      = "X"
        go (Transcription _)        = "Z"

-- (file header, tune header, tune body, inline)
fieldAllowed :: Information -> (Bool, Bool, Bool, Bool)
fieldAllowed = go
    where
        go (Area _)                 = (True, True, False, False)
        go (Book _)                 = (True, True, False, False)
        go (Composer _)             = (True, True, False, False)
        go (Discography _)          = (True, True, False, False)
        go (FileUrl _)              = (True, True, False, False)
        go (Group _)                = (True, True, False, False)
        go (History _)              = (True, True, False, False)
        
        go (Instruction _)          = (True, True, True, True)
        go (Key _)                  = (False, True{-last-}, True, True)
        go (UnitNoteLength _)       = (True, True, True, True)
        go (Meter _)                = (True, True, True, True)
        go Macro                    = (True, True, True, True)
        go (Notes _)                = (True, True, True, True)
        
        go (Origin _)               = (True, True, False, False)
        go Parts                    = (False, True, True, True)
        go (Tempo _)                = (False, True, True, True)
        go (Rhythm _)               = (True, True, True, True)
        go Remark                   = (True, True, True, True)
        
        go (Source _)               = (True, True, False, False)
        go SymbolLine               = (False, False, True, False)
        go (Title _)                = (False, True{-second-}, True, False)
        
        go UserDefined              = (True, True, True, True)
        go (Voice _)                = (False, True, True, True)
        go (Words _)                = (False, True, True, False)
        go (ReferenceNumber _)      = (False, True{-first-}, True, False)
        go (Transcription _)        = (True, True, False, False)

fieldAllowedInFileHeader a = r where (r,_,_,_) = fieldAllowed a
fieldAllowedInTuneHeader a = r where (_,r,_,_) = fieldAllowed a
fieldAllowedInTuneBody   a = r where (_,_,r,_) = fieldAllowed a
fieldAllowedInline       a = r where (_,_,_,r) = fieldAllowed a

showField :: Information -> String
showField = go
    where
        go (Area a)                 = a
        go (Book a)                 = a
        go (Composer a)             = a
        go (Discography a)          = a
        go (FileUrl a)              = a
        go (Group a)                = a
        go (History a)              = a
        go (Instruction a)          = show $ pretty a
        go (Key a)                  = show $ pretty a
        go (UnitNoteLength a)       = show $ pretty a
        go (Meter a)                = show $ pretty a
        go Macro                    = ""
        go (Notes a)                = a
        go (Origin a)               = a
        go Parts                    = "" -- TODO
        go (Tempo a)                = show $ pretty a
        go (Rhythm a)               = a
        go Remark                   = "" -- TODO
        go (Source a)               = a
        go SymbolLine               = "" -- TODO
        go (Title a)                = a
        go UserDefined              = "" -- TODO
        go (Voice a)                = show $ pretty a
        go (Words a)                = a
        go (ReferenceNumber a)      = show a
        go (Transcription a)        = a
        

--------------------------------------------------------------------------------

-- Base types

-- | Pitch (4.1, 4.2).
newtype Pitch = Pitch { getPitch :: (PitchClass, Maybe Accidental, Octave) }
    deriving (Eq, Ord, Show)

instance Pretty Pitch where
    pretty (Pitch (cl, acc, oct)) = pretty acc <> (string $
        (if oct <= 0 then id else fmap toLower) (show cl)
        ++ replicate (negate (fromIntegral oct) `max` 0) ','
        ++ replicate (fromIntegral (oct - 1) `max` 0) '\'')

-- | Pitch class (4.1).
data PitchClass = C | D | E | F | G | A | B
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Accidentals (4.2).
data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty Accidental where
    pretty = go
        where
            go DoubleFlat   = "__"
            go Flat         = "_"
            go Natural      = "="
            go Sharp        = "^"
            go DoubleSharp  = "^^"

-- | Octaves (4.1).
newtype Octave = Octave { getOctave :: Int }
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)






-- | Duration (4.3).
newtype Duration = Duration { getDuration :: Rational }
    deriving (Eq, Ord, Show, Enum, Num, Real, Fractional, RealFrac)

instance Pretty Duration where
    pretty = string . showRatio . getDuration

            
data Meter
    = NoMeter
    | Common
    | Cut
    | Simple Rational
    | Compound [Integer] Integer
    deriving (Eq, Ord, Show)

instance Pretty Meter where
    pretty = go
        where
            go Common           = "C"
            go Cut              = "C|"
            go (Simple a)       = string $ showRatio a
            go (Compound as a)  = sepBy "+" (fmap integer as) <> "/" <> integer a








newtype Key = Key_ (Integer, Mode)
    deriving (Eq, Ord, Show)

instance Pretty Key where
    pretty (Key_ (tonic, mode)) = prettyTonic tonic <+> pretty mode
        where
            prettyTonic a = case a of
                0 -> "C"

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

instance Pretty Mode where
    pretty = go
        where
            go Major        = ""
            go Minor        = "minor"
            go Ionian       = "ionian"
            go Dorian       = "dorian"
            go Phrygian     = "phrygian"
            go Lydian       = "lydian"
            go Mixolydian   = "mixolydian"
            go Aeolian      = "aeolian"
            go Locrian      = "locrian"




-- | Optional string, numerators, frequency (3.1.8)
newtype Tempo = Tempo_ { getTempo :: (Maybe String, [Duration], Duration) }
    deriving (Eq, Ord, Show)

instance Pretty Tempo where
    pretty (Tempo_ (str, durs, bpm)) = 
        pretty str <+> (hsep (fmap pretty durs) <> "=" <> pretty bpm)

data VoiceProperties
    = VoiceProperties
        (Maybe String)
        (Maybe String)
        (Maybe StemDirection)
        (Maybe Clef)
    deriving (Eq, Ord, Show)

instance Pretty VoiceProperties where
    pretty _ = "{VoiceProperties}"
    -- FIXME

data StemDirection = Up | Down
    deriving (Eq, Ord, Show, Enum, Bounded)

data Clef = NoClef | Treble | Alto | Tenor | Bass | Perc
    deriving (Eq, Ord, Show, Enum, Bounded)


-- | Abc directive.
newtype Directive = Directive { getDirective :: (String, String) }
    deriving (Eq, Ord, Show)

instance Pretty Directive where
    pretty _ = "{Directive}"
    -- FIXME



--------------------------------------------------------------------------------

-- Utility

readAbc :: String -> AbcFile
readAbc = error "Not impl"

showAbc :: AbcFile -> String
showAbc = error "Not impl"


showRatio :: (Integral a, Show a) => Ratio a -> String
showRatio x
    | denominator x == 1  = show (numerator x)
    | otherwise           = (show $ numerator x) ++ "/" ++ (show $ denominator x)

