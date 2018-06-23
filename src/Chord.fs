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

let getChordExtension (tonality, notesWithIntervals) =
    let matchExtension extension noteAndInterval =
        match (tonality, extension, snd noteAndInterval) with
        | Major, NoExtension, MajorSixth -> Sixth
        | Major, Sixth, MajorSecond -> SixNine
        | Major, NoExtension, MajorSeventh -> MajorSeven
        | Major, MajorSeven, MajorSecond -> MajorNine
        | Major, MajorSeven, PerfectFourth -> MajorEleven
        | Major, MajorSeven, MajorSixth -> MajorThirteen
        | Major, MajorNine, PerfectFourth -> MajorEleven
        | Major, MajorNine, MajorSixth -> MajorThirteen
        | Major, MajorEleven, MajorSixth -> MajorThirteen
        | _ -> NoExtension
    (tonality, (List.fold(matchExtension) NoExtension) notesWithIntervals, notesWithIntervals)