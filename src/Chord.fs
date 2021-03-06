module Chord

open Scale
open Note

type ChordTonality = 
    | Major 
    | Minor
    | Diminished
    | Augmented
    | Five
    | FlatFifth
    | NoTonality

let private chordTonalityToString = function
    | Major  -> ""
    | Minor -> "m"
    | Diminished -> "°"
    | Augmented -> "+"
    | Five -> "5"
    | FlatFifth -> "(♭5)"
    | NoTonality -> ""

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

let private chordExtensionToString = function
    | Sixth -> "6"
    | SixNine -> "6/9"
    | MajorSeven -> "Maj7"
    | MajorNine -> "Maj9"
    | MajorEleven -> "Maj11"
    | MajorThirteen -> "Maj13"
    | Seven -> "7"
    | Nine -> "9"
    | Eleven -> "11"
    | Thirteen -> "13"
    | AddNine -> "add9"
    | DiminishedSeven -> "dim7"
    | NoExtension -> ""

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

type ChordModifier =
    | Sus2
    | Sus4
    | NoModifier

let private chordModifierToString = function
    | Sus2 -> "sus2"
    | Sus4 -> "sus4"
    | _ -> ""
let getChordModifier (tonality, extension, notesWithIntervals) =
    let matchModifier modifier noteAndInterval =
        match (tonality, modifier, snd noteAndInterval) with
        | Five, _, MajorSecond   -> Sus2
        | Five, _, PerfectFourth -> Sus4
        | _ -> modifier
    match (List.fold(matchModifier) NoModifier notesWithIntervals) with
    | modifier when modifier <> NoModifier -> (NoTonality, extension, modifier, notesWithIntervals)
    | _                                    -> (tonality, extension, NoModifier, notesWithIntervals)

type ChordAlteration =
    | SharpEleven
    | FlatFive
    | SharpFive
    | FlatNine
    | SharpNine
    | NoAlteration

let private chordAlterationToString = function
    | SharpEleven  -> "#11"
    | FlatFive     -> "b5"
    | SharpFive    -> "#5"
    | FlatNine     -> "b9"
    | SharpNine    -> "#9"
    | NoAlteration -> ""

let getChordAlteration (tonality, extension, modifier, notesWithIntervals) =
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
    (tonality, extension, modifier, (List.fold(matchAlteration) NoAlteration notesWithIntervals), notesWithIntervals)

let getChordSlash (tonality, extension, modifier, alteration, notesWithIntervals) =
    let (root, _) = List.head notesWithIntervals
    let (bass, _) = List.minBy(fun ((Note(_, _, _, midi)), _) -> midi) notesWithIntervals
    let slash = match (root, bass) with
                    | r, b when isSameNote r b -> None
                    | _ -> Some bass
    (tonality, extension, modifier, alteration, slash, notesWithIntervals)

let private chordSlashToString = function
    | Some slash -> sprintf "/%s" <| noteToString slash
    | None       -> ""

let getChordOmission (tonality, extension, modifier, alteration, slash, notesWithIntervals) =
    let fifths = [DiminishedFifth; PerfectFifth; MinorSixth]
    let thirds = [MajorSecond; MinorThird; MajorThird; PerfectFourth]
    let omitted5 = notesWithIntervals |> List.filter(fun (_, interval) -> (List.exists(fun interval' -> interval = interval') fifths))
    let omitted3 = notesWithIntervals |> List.filter(fun (_, interval) -> (List.exists(fun interval' -> interval = interval') thirds))
    let omission = match (omitted3, omitted5) with
                    | [], [] -> Some "3, 5"
                    | [], _::_ -> Some "3"
                    | _::_, [] -> Some "5"
                    | _ -> None
    (tonality, extension, modifier, alteration, slash, omission, notesWithIntervals)

let private chordOmissionToString = function
    | Some omission -> sprintf "(omit %s)" omission
    | None          -> ""

let private setup notes =
    let root = List.head notes
    let scale = scaleWithIntervals root
    let noteToInterval = scale |> List.map(fun (interval, note) -> (note, interval)) |> Map.ofList
    List.map(fun note ->
                    let pickInterval note' = function | interval when isSameNote note' note -> Some(interval) |_ -> None
                    (note, Map.pick(pickInterval) noteToInterval)) notes

let getChord =
    setup >> getChordTonality >> getChordExtension >> getChordModifier >> getChordAlteration >> getChordSlash >> getChordOmission

let chordToString (tonality, extension, modifier, alteration, slash, omission, notesWithIntervals) =
    let (root, _) = List.head notesWithIntervals
    sprintf "%s%s%s%s%s%s%s"
                   <| noteToString root
                   <| chordTonalityToString tonality
                   <| chordExtensionToString extension
                   <| chordModifierToString modifier
                   <| chordAlterationToString alteration
                   <| chordSlashToString slash
                   <| chordOmissionToString omission
                