module MajorChordTests

open Xunit
open Scale
open Chord
open TestChord

// chord formulas based on http://www.smithfowler.org/music/Chord_Formulas.htm

[<Fact>]
let ``A triad with a major third, expect a major chord`` () =
    let chordnotes = [(emptyNote, Unison); (emptyNote, MajorThird); (emptyNote, PerfectFifth)]
    let expected = Major
    let (actual, _) = getChordTonality chordnotes
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with a major sixth, expect a 6 chord`` () =
    let chordnotes = [(emptyNote, MajorSixth)]
    let expected = Sixth
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with major sixth and major second interval, expect a 6/9 chord`` () =
    let chordnotes = [(emptyNote, MajorSixth);(emptyNote, MajorSecond) ]
    let expected = SixNine
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with major seventh interval, expect a Maj7 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh) ]
    let expected = MajorSeven
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with major seventh and major second interval, expect a Maj9 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh); (emptyNote, MajorSecond) ]
    let expected = MajorNine
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with major seventh, major second and perfect fourth interval, expect a Maj11 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh);
                                              (emptyNote, MajorSecond);
                                              (emptyNote, PerfectFourth) ]
    let expected = MajorEleven
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with major seventh and perfect fourth interval, expect a Maj11 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh);
                                              (emptyNote, PerfectFourth) ]
    let expected = MajorEleven
    let (_, actual, _)= getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with major seventh, major second, perfect fourth, major sixth interval, expect a Maj13 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh);
                                              (emptyNote, MajorSecond);
                                              (emptyNote, PerfectFourth);
                                              (emptyNote, MajorSixth) ]
    let expected = MajorThirteen
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with major seventh, major second, major sixth interval, expect a Maj13 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh);
                                              (emptyNote, MajorSecond);
                                              (emptyNote, MajorSixth) ]
    let expected = MajorThirteen
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A major chord with major seventh and major sixth interval, expect a Maj13 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh);
                                              (emptyNote, MajorSixth) ]
    let expected = MajorThirteen
    let (_, actual, _) = getChordExtension (Major, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A Maj7 chord with a diminished fifth interval, expect a Maj7#11 chord`` () =
    let chordnotes = [(emptyNote, PerfectFifth); (emptyNote, DiminishedFifth) ]
    let expected = SharpEleven
    let (_, _, actual, _) = getChordAlteration (Major, MajorSeven, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A Major chord with a diminished fith interval, expect a Major flat five chord`` () =
    let chordnotes = [(emptyNote, DiminishedFifth) ]
    let expected = FlatFive
    let (_, _, actual, _) = getChordAlteration (Major, NoExtension, chordnotes)
    Assert.Equal(expected, actual) 