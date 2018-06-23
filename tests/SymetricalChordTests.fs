module SymetricalChordTests

open Xunit
open Scale
open Chord
open TestChord

// chord formulas based on http://www.smithfowler.org/music/Chord_Formulas.htm

[<Fact>]
let ``A triad with a minor third and diminished fifth, expect a dim chord`` () =
    let chordnotes = [(emptyNote, Unison); (emptyNote, MinorThird); (emptyNote, DiminishedFifth)]
    let expected = Diminished
    let (actual, _) = getChordTonality chordnotes
    Assert.Equal(expected, actual)

[<Fact>]
let ``A diminished chord with a double flat 7, expect a dim7 chord`` () =
    let chordnotes = [(emptyNote, MajorSixth)]
    let expected = DiminishedSeven
    let (_, actual, _) = getChordExtension (Diminished, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A triad with a major third and a minor sixth, expect a aug chord`` () =
    let chordnotes = [(emptyNote, Unison); (emptyNote, MajorThird); (emptyNote, MinorSixth)]
    let expected = Augmented
    let (actual, _) = getChordTonality chordnotes
    Assert.Equal(expected, actual)
