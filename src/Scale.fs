module Scale

open Note

// https://en.m.wikipedia.org/wiki/Chord_names_and_symbols_(popular_music)#Intervals
type Interval =
    | Unison            // 0
    | MinorSecond       // 1
    | MajorSecond       // 2
    | MinorThird        // 3
    | MajorThird        // 4
    | PerfectFourth     // 5
    | DiminishedFifth   // 6
    | PerfectFifth      // 7
    | MinorSixth        // 8
    | MajorSixth        // 9
    | MinorSeventh      // 10
    | MajorSeventh      // 11
    | Octave            // 12

let intervals = [
                         Unison
                         MinorSecond
                         MajorSecond
                         MinorThird
                         MajorThird
                         PerfectFourth
                         DiminishedFifth
                         PerfectFifth
                         MinorSixth
                         MajorSixth
                         MinorSeventh
                         MajorSeventh
                        ]

let scaleWithIntervals tonic =
    let accidental = match tonic with | Note(_, Natural, _, _) -> Sharp | Note(_, acc, _ ,_) -> acc
    let chromatic = List.filter(fun (Note(_, accidental', _, _)) -> accidental' = accidental || accidental' = Natural) allNotes
    let length = List.length chromatic
    let idx = List.findIndex(fun note -> isSameNote note tonic) chromatic
    List.permute(fun idx' -> (idx' + (length - idx)) % length) chromatic |> List.zip intervals