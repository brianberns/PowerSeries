namespace Bernsrite.PowerSeries

open System.Numerics
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type UnitTest() =

    [<TestMethod>]
    member __.Test1() =

        let coeff = [1; 0; -2]
        let expected = [1; 0; -6; 0; 12; 0; -8; 0; 0; 0]

        let cast = id
        let series =
            coeff |> List.map cast |> PowerSeries.ofList
        let series = series ** 3
        Assert.AreEqual(
            expected |> List.map cast,
            series |> PowerSeries.take 10)

        let cast = int64
        let series =
            coeff |> List.map cast |> PowerSeries.ofList
        let series = series ** 3
        Assert.AreEqual(
            expected |> List.map cast,
            series |> PowerSeries.take 10)

        let cast (x : int) = BigInteger(x)
        let series =
            coeff |> List.map cast |> PowerSeries.ofList
        let series = series ** 3
        Assert.AreEqual(
            expected |> List.map cast,
            series |> PowerSeries.take 10)
