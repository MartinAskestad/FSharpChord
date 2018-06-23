module DominantChordTests

open Xunit
open Scale
open Chord
open TestChord

// chord formulas based on http://www.smithfowler.org/music/Chord_Formulas.htm

[<Fact>]
let ``A major chord with a minor seventh interval, expect a 7 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh)]
    let expected = Seven
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with a minor seventh and major second interval, expect a 9 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh);
                                              (emptyNote, MajorSecond)]
    let expected = Nine
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A chord with minor seventh, major second and perfect fourth interval, expect a 11 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh);
                                              (emptyNote, MajorSecond);
                                              (emptyNote, PerfectFourth)]
    let expected = Eleven
    let (_, actual, _) = getChordExtension (NoTonality, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A chord with a minor seventh and perfect fourth interval, expect a 11 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh);
                                              (emptyNote, PerfectFourth)]
    let expected = Eleven
    let (_, actual, _) = getChordExtension (NoTonality, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with  a minor seventh and perfect fourth interval, expect a 11 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh);
                                              (emptyNote, PerfectFourth)]
    let expected = Eleven
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with  a minor seventh, major second and perfect fourth interval, expect a 11 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh);
                                              (emptyNote, MajorSecond);
                                              (emptyNote, PerfectFourth)]
    let expected = Eleven
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with a minor seventh and major sixth interval, expect a 13 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh);
                                              (emptyNote, MajorSixth)]
    let expected = Thirteen
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with a minor seventh and, major second, major sixth interval, expect a 13 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh);
                                              (emptyNote, MajorSecond);
                                              (emptyNote, MajorSixth)]
    let expected = Thirteen
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with a minor seventh and, major second, perfect fourth, major sixth interval, expect a 13 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh);
                                              (emptyNote, MajorSecond);
                                              (emptyNote, PerfectFourth);
                                              (emptyNote, MajorSixth)]
    let expected = Thirteen
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A chord with a augmented fifth and minor seventh, expect a #5 chord`` () =
    let chordnotes = [(emptyNote, MinorSixth); (emptyNote, MinorSeventh)]
    let expected = SharpFive
    let (_, _, actual, _) = getChordAlteration (NoTonality, NoExtension, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A chord with a diminished fifth expect a b5 chord`` () =
    let chordnotes = [(emptyNote, DiminishedFifth)]
    let expected = FlatFive
    let (_, _, actual, _) = getChordAlteration (NoTonality, NoExtension, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A chord with a minor second expect a b9 chord`` () =
    let chordnotes = [(emptyNote, MinorSecond)]
    let expected = FlatNine
    let (_, _, actual, _) = getChordAlteration (NoTonality, NoExtension, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A chord with a minor third expect a #9 chord`` () =
    let chordnotes = [(emptyNote, MinorThird)]
    let expected = SharpNine
    let (_, _, actual, _) = getChordAlteration (NoTonality, NoExtension, chordnotes)
    Assert.Equal(expected, actual)
