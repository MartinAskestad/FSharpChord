module Chord

open Note
open Scale

type ChordTonality = | Major | NoTonality

let getChordTonality notesWithIntervals =
    let matchTonality tonality noteAndInterval =
        match (tonality, snd noteAndInterval) with
        | NoTonality, MajorThird -> Major
        | _                      -> tonality
    ((List.fold(matchTonality) NoTonality notesWithIntervals), notesWithIntervals)

type ChordExtension =
    | Sixth
    | SixNine
    | MajorSeven
    | MajorNine
    | MajorEleven
    | MajorThirteen
    | NoExtension

let (|Major7Extension|_|) = function
    | MajorSeven    -> Some()
    | MajorNine     -> Some()
    | MajorEleven   -> Some()
    | MajorThirteen -> Some()
    | _             -> None

let getChordExtension (tonality, notesWithIntervals) =
    let matchExtension extension noteAndInterval =
        match (tonality, extension, snd noteAndInterval) with
        | Major, NoExtension, MajorSixth        -> Sixth
        | Major, Sixth, MajorSecond             -> SixNine
        | Major, NoExtension, MajorSeventh      -> MajorSeven
        | Major, Major7Extension, MajorSecond   -> MajorNine
        | Major, Major7Extension, PerfectFourth -> MajorEleven
        | Major, Major7Extension, MajorSixth    -> MajorThirteen
        | _                                     -> NoExtension
    (tonality, (List.fold(matchExtension) NoExtension) notesWithIntervals, notesWithIntervals)

type ChordAlteration =
    | SharpEleven
    | FlatFive
    | NoAlteration

let getChordAlteration (tonality, extension, notesWithIntervals) =
    let has interval = List.exists(fun (_, interval') -> interval' = interval) notesWithIntervals
    let matchAlteration alteration noteAndInterval =
        match (tonality, alteration, snd noteAndInterval) with
        | Major, _, DiminishedFifth -> if has PerfectFifth then SharpEleven else FlatFive
        | _ -> alteration
    (tonality, extension, (List.fold(matchAlteration) NoAlteration notesWithIntervals), notesWithIntervals)