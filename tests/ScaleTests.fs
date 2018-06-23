module ScaleTests

open System
open System.Collections.Generic
open Xunit
open Scale
open Note
open System.Collections

[<Fact>]
let ``Expect a chromatic scale with intervals`` () =
    let expected = [(Unison, E); (MinorSecond, F); (MajorSecond, F'); (MinorThird, G); (MajorThird, G'); (PerfectFourth, A); (DiminishedFifth, A'); (PerfectFifth, B); (MinorSixth, C); (MajorSixth, C'); (MinorSeventh, D); (MajorSeventh, D')]
    let actual = scaleWithIntervals E
    Assert.Equal<IEnumerable<Tuple<Interval, Note>>>(expected, actual)

[<Fact>]
let ``Expect a chromatic scale with intervals and flatted notes when a flat note is tonic`` () =
    let expected = [(Unison, Bb); (MinorSecond, B); (MajorSecond, C); (MinorThird, Db); (MajorThird, D); (PerfectFourth, Eb); (DiminishedFifth, E); (PerfectFifth, F); (MinorSixth, Gb); (MajorSixth, G); (MinorSeventh, Ab); (MajorSeventh, A)]
    let actual = scaleWithIntervals Bb
    Assert.Equal<IEnumerable<Tuple<Interval, Note>>>(expected, actual)