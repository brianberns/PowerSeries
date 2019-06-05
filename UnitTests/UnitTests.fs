namespace Bernsrite.PowerSeries

open System.Numerics
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type UnitTest() =

    let x = PowerSeries.x

    [<TestMethod>]
    member __.Types() =

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

    [<TestMethod>]
    member __.Expression() =

        let series = (1G - (2G * (x * x))) ** 3
        Assert.AreEqual(
            [1; 0; -6; 0; 12; 0; -8; 0; 0; 0],
            series |> PowerSeries.take 10)

        let series = 1G / (1G - x)
        Assert.AreEqual(
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 1],
            series |> PowerSeries.take 10)

        let series = 1G / (1G - x) ** 2
        Assert.AreEqual(
            [1; 2; 3; 4; 5; 6; 7; 8; 9; 10],
            series |> PowerSeries.take 10)

    [<TestMethod>]
    member __.Calculus() =

        let series = x * x |> PowerSeries.deriv
        Assert.AreEqual(
            [0; 2; 0],
            series |> PowerSeries.take 3)

        let series = series |> PowerSeries.integral
        Assert.AreEqual(
            [0; 0; 1],
            series |> PowerSeries.take 3)
