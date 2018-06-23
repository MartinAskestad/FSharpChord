module Chord

open Scale

type ChordTonality = 
    | Major 
    | Minor
    | Diminished
    | Augmented
    | Five
    | FlatFifth
    | NoTonality

let getChordTonality notesWithIntervals =
    let matchTonality tonality noteAndInterval =
        match (tonality, snd noteAndInterval) with
        | _,              MajorThird         -> Major
        | _,              MinorThird         -> Minor
        | NoTonality,     PerfectFifth       -> Five
        | NoTonality,     DiminishedFifth    -> FlatFifth
        | Minor,          DiminishedFifth    -> Diminished
        | Major,          MinorSixth         -> Augmented
        | _                                  -> tonality
    ((List.fold(matchTonality) NoTonality notesWithIntervals), notesWithIntervals)

type ChordExtension =
    | Sixth
    | SixNine
    | MajorSeven
    | MajorNine
    | MajorEleven
    | MajorThirteen
    | Seven
    | Nine
    | Eleven
    | Thirteen
    | AddNine
    | DiminishedSeven
    | NoExtension

let (|Major7Extension|_|) = function
    | MajorSeven    -> Some()
    | MajorNine     -> Some()
    | MajorEleven   -> Some()
    | MajorThirteen -> Some()
    | _             -> None

let (|Minor7Extension|_|) = function
    | Seven -> Some()
    | Nine -> Some()
    | Eleven -> Some()
    | Thirteen -> Some()
    | _         -> None

let getChordExtension (tonality, notesWithIntervals) =
    let matchExtension extension noteAndInterval =
        match (tonality, extension, snd noteAndInterval) with
        | Diminished, NoExtension,     MajorSixth    -> DiminishedSeven
        | _,          NoExtension,     MajorSixth    -> Sixth
        | _,          Sixth,           MajorSecond   -> SixNine
        | _,          NoExtension,     MajorSeventh  -> MajorSeven
        | _,          Major7Extension, MajorSecond   -> MajorNine
        | _,          Major7Extension, PerfectFourth -> MajorEleven
        | _,          Major7Extension, MajorSixth    -> MajorThirteen
        | _,          NoExtension,     MinorSeventh  -> Seven
        | Minor,      NoExtension,     MajorSecond   -> AddNine
        | _,          Minor7Extension, MajorSecond   -> Nine
        | _,          Minor7Extension, PerfectFourth -> Eleven
        | _,          Minor7Extension, MajorSixth    -> Thirteen
        | _                                          -> NoExtension
    (tonality, (List.fold(matchExtension) NoExtension) notesWithIntervals, notesWithIntervals)

type ChordAlteration =
    | SharpEleven
    | FlatFive
    | SharpFive
    | FlatNine
    | SharpNine
    | NoAlteration

let getChordAlteration (tonality, extension, notesWithIntervals) =
    let has interval = List.exists(fun (_, interval') -> interval' = interval) notesWithIntervals
    let matchAlteration alteration noteAndInterval =
        match (tonality, alteration, snd noteAndInterval) with
        | _, _, DiminishedFifth -> if has PerfectFifth then
                                                             SharpEleven else
                                                             FlatFive
        | _, _, MinorSixth                               ->  SharpFive
        | _, _, MinorSecond                              ->  FlatNine
        | tonality, _, MinorThird when tonality <> Minor -> SharpNine
        | _                                              -> alteration
    (tonality, extension, (List.fold(matchAlteration) NoAlteration notesWithIntervals), notesWithIntervals)

type ChordModifier =
    | Sus4
    | NoModifier

let getChordModifier (tonality, notesWithIntervals)=
    let matchModifier modifier noteAndInterval =
        match (tonality, modifier, snd noteAndInterval) with
        | Five, _, PerfectFourth -> Sus4
        | _ -> modifier
    (tonality, (List.fold(matchModifier) NoModifier notesWithIntervals), notesWithIntervals)