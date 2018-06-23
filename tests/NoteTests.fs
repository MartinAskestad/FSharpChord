module NoteTests

open Xunit
open Note

[<Fact>]
let ``Expect true when two notes are the same`` () =
    let firstNote = C
    let secondNote = Note('C', Natural, 0, 0)
    Assert.True(isSameNote firstNote secondNote)

[<Fact>]
let ``When getting the name of a note, expect the name as a string`` () =
    let noteName = getNoteName C
    Assert.Equal("C", noteName)

[<Fact>]
let ``When getting the accidental of a note, expect the accidental`` () =
    let accidental = getNoteAccidental C
    Assert.Same(Natural, accidental)

[<Fact>]
let ``When getting the note number, expect the note number`` () =
    let index = getNoteNumber C
    Assert.Equal(0, index)

[<Fact>]
let ``When getting the note midi index, expect the note midi index`` () =
    let index = getNoteMidiIndex C
    Assert.Equal(0, index)

[<Fact>]
let ``Expect a C note`` () =
    let actual = noteFromString "C"
    Assert.Equal(Some C, actual)

[<Fact>]
let ``Expect the open notes from a guitar in standard tuning`` () =
    let actualE = noteFromMidi 28.0
    let actualB = noteFromMidi 23.0
    let actualG = noteFromMidi 19.0
    let actualD = noteFromMidi 14.0
    let actualA = noteFromMidi 9.0
    let actualE2 = noteFromMidi 4.0
    Assert.Equal(Note('E', Natural, 4, 28), actualE)
    Assert.Equal(Note('B', Natural, 11, 23), actualB)
    Assert.Equal(Note('G', Natural, 7, 19), actualG)
    Assert.Equal(Note('D', Natural, 2, 14), actualD)
    Assert.Equal(Note('A', Natural, 9, 9), actualA)
    Assert.Equal(Note('E', Natural, 4, 4), actualE2)