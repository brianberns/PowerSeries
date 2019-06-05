namespace Bernsrite.PowerSeries

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type UnitTest() =

    [<TestMethod>]
    member __.Test1() =
        let series = [1; 0; -2] |> PowerSeries.ofList
        let series = series ** 3
        Assert.AreEqual(
            [1; 0; -6; 0; 12; 0; -8; 0; 0; 0],
            series |> PowerSeries.take 10)
