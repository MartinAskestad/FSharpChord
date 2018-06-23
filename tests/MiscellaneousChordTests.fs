module MiscellaneousChordTests

open Xunit
open Note
open Scale
open Chord
open TestChord

[<Fact>]
let ``A chord with ony unison and perfect fith, expect a Five chord`` () =
    let chordtones = [(emptyNote, Unison); (emptyNote, PerfectFifth)]
    let expected = Five
    let (actual, _) = getChordTonality chordtones
    Assert.Equal(expected, actual)


[<Fact>]
let ``A chord with only unison and diminished fifth, expect a flat five chord`` () =
    let chordtones = [(emptyNote, Unison); (emptyNote, DiminishedFifth)]
    let expected = FlatFifth
    let (actual, _) = getChordTonality chordtones
    Assert.Equal(expected, actual)

[<Fact>]
let ``A chord with perfect fourth and a perfect fifth, expect a sus4 chord`` () =
    let chordnotes = [(emptyNote, PerfectFourth); (emptyNote, PerfectFifth) ]
    let expected = Sus4
    let (_, actual, _) = getChordModifier (Five, chordnotes)
    Assert.Equal(expected, actual)