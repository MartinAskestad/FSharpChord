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
    let (_, _, actual, _) = getChordModifier (Five, NoExtension, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A chord with major second and perfect fifth, expect a sus2 chord`` () =
    let chordnotes = [(emptyNote, MajorSecond); (emptyNote, PerfectFifth) ]
    let expected = Sus2
    let (_, _, actual, _) = getChordModifier (Five, NoExtension, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A chord with perfect fifth and a diminished fifth, expect a #11 chord`` () =
    let chordnotes = [(emptyNote, PerfectFifth); (emptyNote, DiminishedFifth) ]
    let expected = SharpEleven
    let (_, _, _, actual, _) = getChordAlteration (NoTonality, NoExtension, NoModifier, chordnotes)
    Assert.Equal(expected, actual)

[<Fact>]
let ``A chord with bass note other than root, expect a slash chord`` () =
    let chordnotes = [(Note('C', Natural, 0, 12), Unison)
                      (Note('E', Natural, 4, 16), MajorThird)
                      (Note('G', Natural, 7, 7), PerfectFifth) ]
    let expected = Some (Note('G', Natural, 7, 7)) 
    let actual = getChordSlash chordnotes
    Assert.Equal(expected, actual)

[<Fact>]
let ``A chord without a fifth, expect omitted fifth`` () =
    let chordnotes =  [(emptyNote, Unison)
                       (emptyNote, PerfectFourth)
                       (emptyNote, MajorSixth) ]
    let expected = Some "5" 
    let actual = getChordOmission chordnotes
    Assert.Equal(expected, actual)

[<Fact>]
let ``A chord without a third, expect omitted third`` () =
    let chordnotes = [(emptyNote, Unison)
                      (emptyNote, PerfectFifth) ]
    let expected = Some "3"
    let actual = getChordOmission chordnotes
    Assert.Equal(expected, actual)

[<Fact>]
let ``A chord without a thir or a fifth, expect a omitted 3, 5`` () =
    let chordnotes = [(emptyNote, Unison)]
    let expected = Some "3, 5"
    let actual = getChordOmission chordnotes
    Assert.Equal(expected, actual)
