

module Main where

import System.Process
import Music.Abc
import Text.Pretty (pretty)

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
    (Just $ FileHeader [
        Title "Collection"        
    ] []) 
    [
        Tune (AbcTune 
            (TuneHeader [
                ReferenceNumber     19004,
                -- Title               "Silent Night",
                -- Title               "Stille Nacht! Heilige Nacht!",
                -- Rhythm              "Air",
                -- Composer            "Franz Xaver Gruber, 1818",
                -- Origin              "Austria",
                -- Source              "Paul Hardy's Xmas Tunebook 2012",
                Meter               (Simple $ 6/8),
                UnitNoteLength      (1/8),
                -- Tempo               (Tempo_ (Just "Andante", [3/8], 60)),
                Key                 (Key_ (0, Minor))            

                -- Words               "Silent night, holy night",
                -- Words               "All is calm, all is bright",
                -- Words               "Round yon Virgin Mother and Child",
                -- Words               "Holy Infant so tender and mild",
                -- Words               "Sleep in heavenly peace",
                -- Words               "Sleep in heavenly peace"
            ]) 
            [
                Sequence [
                  Chord (Chord_ ([(Pitch (C,Just Sharp,0))], Just 1)),
                  Chord (Chord_ ([(Pitch (D,Nothing,0))], Just 1)),
                  Chord (Chord_ ([(Pitch (E,Nothing,0))], Just 1)),
                  Chord (Chord_ ([(Pitch (F,Nothing,0))], Just 1)),
                  Chord (Chord_ ([(Pitch (D,Nothing,0))], Just 1)),
                  Chord (Chord_ ([(Pitch (E,Nothing,0))], Just 1)),
                  Chord (Chord_ ([(Pitch (F,Nothing,0))], Just 1)),
                  Chord (Chord_ ([(Pitch (D,Nothing,0))], Just 1)),
                  Chord (Chord_ ([(Pitch (E,Nothing,0))], Just 1)),
                  Chord (Chord_ ([(Pitch (F,Nothing,0))], Just 1)),
                  Chord (Chord_ ([(Pitch (D,Nothing,0))], Just 1)),
                  Chord (Chord_ ([(Pitch (E,Nothing,0))], Just 1)),
                  Chord (Chord_ ([(Pitch (F,Nothing,0))], Just 1)),
                  Chord (Chord_ ([(Pitch (C,Nothing,1))], Just 1))
                  ]
            ])

    ]

main = do
  let abc = show $ pretty test
  -- print $ abc
  writeFile "test.abc" abc
  system "osascript -e 'tell application \"Google Chrome\" to tell the active tab of its first window' -e 'reload' -e 'end tell'"
  



