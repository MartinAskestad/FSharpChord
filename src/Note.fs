module Note

open System

type Accidental = Flat | Natural | Sharp
type Note = Note of char * Accidental * int * int

let B' = Note('B', Sharp, 0, 0)
let C  = Note('C', Natural, 0, 0)
let C' = Note('C', Sharp, 1, 0)
let Db = Note('D', Flat, 1, 0)
let D  = Note('D', Natural, 2, 0)
let D' = Note('D', Sharp, 3, 0)
let Eb = Note('E', Flat, 3, 0)
let E  = Note('E', Natural, 4, 0)
let E' = Note('E', Sharp, 5, 0)
let Fb = Note('F', Flat, 4, 0)
let F  = Note('F', Natural, 5, 0)
let F' = Note('F', Sharp, 6, 0)
let Gb = Note('G', Flat, 6, 0)
let G  = Note('G', Natural, 7, 0)
let G' = Note('G', Sharp, 8, 0)
let Ab = Note('A', Flat, 8, 0)
let A  = Note('A', Natural, 9, 0)
let A' = Note('A', Sharp, 10, 0)
let Bb = Note('B', Flat, 10, 0)
let B  = Note('B', Natural, 11, 0)
let Cb = Note('C', Flat, 11, 0)

let allNotes = [C; C'; Db; D; D'; Eb; E; F; F'; Gb; G; G'; Ab; A; A'; Bb; B]

let isSameNote (Note(_, _ ,idxA, _)) (Note(_, _, idxB, _)) = idxA = idxB
let getNoteName (Note(name, _, _, _)) = string name
let getNoteAccidental (Note(_, accidental, _, _)) = accidental
let getNoteNumber (Note(_, _, number, _)) = number
let getNoteMidiIndex (Note(_, _, _, midi)) = midi

let private uncons (str:string) = (str.[0], str.Substring(1))

let noteFromString (noteString:string) =
    match uncons noteString with
    | ('C', "") -> Some C
    | ('D', "") -> Some D
    | ('E', "") -> Some E
    | ('F', "") -> Some F
    | ('G', "") -> Some G
    | ('A', "") -> Some A
    | ('B', "") -> Some B
    | ('C', acc) when acc = "#" || acc = "♯" -> Some C'
    | ('D', acc) when acc = "#" || acc = "♯" -> Some D'
    | ('E', acc) when acc = "#" || acc = "♯" -> Some E'
    | ('F', acc) when acc = "#" || acc = "♯" -> Some F'
    | ('G', acc) when acc = "#" || acc = "♯" -> Some G'
    | ('A', acc) when acc = "#" || acc = "♯" -> Some A'
    | ('B', acc) when acc = "#" || acc = "♯" -> Some B'
    | ('C', acc) when acc = "b" || acc = "♭" -> Some Cb
    | ('D', acc) when acc = "b" || acc = "♭" -> Some Db
    | ('E', acc) when acc = "b" || acc = "♭" -> Some Eb
    | ('F', acc) when acc = "b" || acc = "♭" -> Some Fb
    | ('G', acc) when acc = "b" || acc = "♭" -> Some Gb
    | ('A', acc) when acc = "b" || acc = "♭" -> Some Ab
    | ('B', acc) when acc = "b" || acc = "♭" -> Some Bb
    | _ -> None

let noteFromMidi (midi:float) =
    let midi' = int(Math.Round(midi))
    let sharps = allNotes |> List.filter(fun (Note(_, accidental, _, _)) -> accidental = Sharp || accidental = Natural)
    let (Note(name, accidental, idx, _)) = sharps.[midi' % 12]
    Note(name, accidental, idx, midi')