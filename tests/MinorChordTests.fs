module MinorChordTests

open Xunit
open Scale
open Chord
open TestChord

// chord formulas based on http://www.smithfowler.org/music/Chord_Formulas.htm

[<Fact>]
let ``A triad with a minor third, expect a minor chord`` () =
    let chordnotes = [(emptyNote, Unison); (emptyNote, MinorThird); (emptyNote, PerfectFifth)]
    let expected = Minor
    let (actual,_ ) = getChordTonality chordnotes
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with major sixth interval, expect a m6 chord`` () =
    let chordnotes = [(emptyNote, MajorSixth) ]
    let expected = Sixth
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with minor seventh interval, expect a m7 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh) ]
    let expected = Seven
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with major second interval, expect a madd9 chord`` () =
    let chordnotes = [(emptyNote, MajorSecond) ]
    let expected = AddNine
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with major sixth and major second interval, expect a m6/9 chord`` () =
    let chordnotes = [(emptyNote, MajorSixth);
                                              (emptyNote, MajorSecond) ]
    let expected = SixNine
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with a minor seventh, major sixth and major second, expect a m9 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh);
                                              (emptyNote, MajorSecond) ]
    let expected = Nine
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with a minor seventh, major sixth, major second and a perfect fourth interval, expect a m11 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh);
                                              (emptyNote, MajorSecond);
                                              (emptyNote, PerfectFourth) ]
    let expected = Eleven
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with a minor seventh, major sixth, and a perfect fourth interval, expect a m11 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh);
                                              (emptyNote, PerfectFourth) ]
    let expected = Eleven
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with a minor seventh, major sixth, major second, perfect fourth and a major sixth interval, expect a m13 chord`` () =
    let chordnotes = [(emptyNote, MinorSeventh);
                                              (emptyNote, MajorSecond);
                                              (emptyNote, PerfectFourth);
                                              (emptyNote, MajorSixth) ]
    let expected = Thirteen
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with major seventh interval, expect a m/Maj7 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh) ]
    let expected = MajorSeven
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with major seventh and major second interval, expect a m/Maj9 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh);
                                              (emptyNote, MajorSecond) ]
    let expected = MajorNine
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with major seventh, major second, and a perfect fourth interval, expect a m/Maj11 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh);
                                              (emptyNote, MajorSecond);
                                              (emptyNote, PerfectFourth) ]
    let expected = MajorEleven
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with major seventh, and a perfect fourth interval, expect a m/Maj11 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh);
                                              (emptyNote, PerfectFourth) ]
    let expected = MajorEleven
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with major seventh, major second, perfect fourth and a major sixth interval, expect a m/Maj13 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh);
                                              (emptyNote, MajorSecond);
                                              (emptyNote, PerfectFourth);
                                              (emptyNote, MajorSixth) ]
    let expected = MajorThirteen
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with major seventh, major second, and a major sixth interval, expect a m/Maj13 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh);
                                              (emptyNote, MajorSecond);
                                              (emptyNote, MajorSixth) ]
    let expected = MajorThirteen
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with major seventh, and a major sixth interval, expect a m/Maj13 chord`` () =
    let chordnotes = [(emptyNote, MajorSeventh);
                                              (emptyNote, MajorSixth) ]
    let expected = MajorThirteen
    let (_, actual, _) = getChordExtension (Minor, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A minor chord with a diminished fith and a minor seventh interval, expect a m7b5 chord`` () =
    let chordnotes = [(emptyNote, DiminishedFifth); (emptyNote, MinorSeventh)]
    let expected = FlatFive
    let (_, _, _, actual, _) = getChordAlteration (Major, Seven, NoModifier, chordnotes)
    Assert.Equal(expected, actual)