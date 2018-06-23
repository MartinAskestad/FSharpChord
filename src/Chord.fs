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