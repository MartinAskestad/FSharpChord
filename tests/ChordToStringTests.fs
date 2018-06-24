module ChordToStringTests

open Xunit
open Note
open Chord
open Chord
open MinorChordTests

[<Fact>]
let ``A C major chord, expect "C"`` () =
    let cmaj = (Major, NoExtension, NoModifier, NoAlteration, None, None, [(C, 0)])
    let expected = "C"
    let actual = chordToString cmaj
    Assert.Equal(expected, actual)

[<Fact>]
let ``A C minor chord, expect a "Cm"`` () =
    let cmin = (Minor, NoExtension, NoModifier, NoAlteration, None, None, [(C, 0)])
    let expected = "Cm"
    let actual = chordToString cmin
    Assert.Equal(expected, actual)

[<Fact>]
let ``A C flat five chord, expect a "C(b5)"`` () =
    let cmin = (FlatFifth, NoExtension, NoModifier, NoAlteration, None, None, [(C, 0)])
    let expected = "C(♭5)"
    let actual = chordToString cmin
    Assert.Equal(expected, actual)

[<Fact>]
let ``A C five chord, expect a "C5"`` () =
    let cmin = (Five, NoExtension, NoModifier, NoAlteration, None, None, [(C, 0)])
    let expected = "C5"
    let actual = chordToString cmin
    Assert.Equal(expected, actual)

[<Fact>]
let ``A C aug chord, expect a "C+"`` () =
    let cmin = (Augmented, NoExtension, NoModifier, NoAlteration, None, None, [(C, 0)])
    let expected = "C+"
    let actual = chordToString cmin
    Assert.Equal(expected, actual)

[<Fact>]
let ``A C dim chord, expect a "C°"`` () =
    let cmin = (Diminished, NoExtension, NoModifier, NoAlteration, None, None, [(C, 0)])
    let expected = "C°"
    let actual = chordToString cmin
    Assert.Equal(expected, actual)

[<Fact>]
let ``A C7 chord, expect a "C7"`` () =
    let cseven = (Major, Seven, NoModifier, NoAlteration, None, None, [(C, 0)])
    let expected = "C7"
    let actual = chordToString cseven
    Assert.Equal(expected, actual)

[<Fact>]
let ``A C sus4 chord, expect "Csus4"`` () =
    let csus4 = (NoTonality, NoExtension, Sus4, NoAlteration, None, None, [(C, 0)])
    let expected ="Csus4"
    let actual = chordToString csus4
    Assert.Equal(expected, actual)

[<Fact>]
let ``A C#9 chord, expect "C#9`` () =
    let csharp9 = (Major, NoExtension, NoModifier, SharpNine, None, None, [(C, 0)])
    let actual = chordToString csharp9
    Assert.Equal("C#9", actual)
    
[<Fact>]
let ``A A/E chord, expect A/E`` () =
    let aSlashE = (Major, NoExtension, NoModifier, NoAlteration, Some E, None, [(A, 0)])
    let expected = "A/E"
    let actual = chordToString aSlashE
    Assert.Equal(expected, actual)

[<Fact>]
let ``A C omit 5 chord, expect "C(omit 5)"`` () =
    let cOmit5 = (Major, NoExtension, NoModifier, NoAlteration, None, Some "5", [(C, 0)])
    let actual = chordToString cOmit5
    Assert.Equal("C(omit 5)", actual)