﻿namespace Bernsrite.PowerSeries

open System
open System.Numerics

open Microsoft.VisualStudio.TestTools.UnitTesting

open MathNet.Numerics

[<TestClass>]
type UnitTest() =

    let x = PowerSeries.X

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

        let cast = decimal
        let series =
            coeff |> List.map cast |> PowerSeries.ofList
        let series = series ** 3
        Assert.AreEqual(
            expected |> List.map cast,
            series |> PowerSeries.take 10)

        let cast = BigRational.FromInt
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

    [<TestMethod>]
    member __.Exp() =
        Assert.AreEqual(
            [1N; 1N; 1N/2N; 1N/6N; 1N/24N; 1N/120N; 1N/720N],
            PowerSeries.exp |> PowerSeries.take 7)
        Assert.AreEqual(
            Math.E,
            PowerSeries.exp
                |> PowerSeries.eval 100 1N
                |> float)

    [<TestMethod>]
    member __.Trig() =
        printfn "%A" (PowerSeries.sin |> PowerSeries.take 30)
        printfn "%A" (PowerSeries.cos |> PowerSeries.take 30)

    /// https://www.emathzone.com/tutorials/calculus/maclaurin-series-of-sqrt1x.html
    [<TestMethod>]
    member __.Sqrt() =
        let x = PowerSeries<BigRational>.X
        Assert.AreEqual(
            [1N; 1N/2N; -1N/8N; 1N/16N],
            (1Z + x)
                |> PowerSeries.sqrt
                |> PowerSeries.take 30)
