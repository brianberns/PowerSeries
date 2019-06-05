namespace Bernsrite.PowerSeries

open System
open System.Numerics

open Microsoft.VisualStudio.TestTools.UnitTesting

open MathNet.Numerics

[<TestClass>]
type UnitTest() =

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

        let x = PowerSeries<int>.X
        let series = (1 - (2 * (x ** 2))) ** 3
        printfn "%A" <| PowerSeries.toString series
        Assert.AreEqual(
            [1; 0; -6; 0; 12; 0; -8; 0; 0; 0],
            series |> PowerSeries.take 10)

        let series = 1 / (1 - x)
        Assert.AreEqual(
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 1],
            series |> PowerSeries.take 10)

        let series = 1 / (1 - x) ** 2
        Assert.AreEqual(
            [1; 2; 3; 4; 5; 6; 7; 8; 9; 10],
            series |> PowerSeries.take 10)

    [<TestMethod>]
    member __.Calculus() =
 
        let x = PowerSeries<int>.X
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

        let test seriesA seriesB =
            let take = PowerSeries.take 30
            Assert.AreEqual(
                take seriesA,
                take seriesB)

        test
            PowerSeries.sin
            (PowerSeries.sqrt (1N - PowerSeries.cos ** 2))

        let x = PowerSeries<BigRational>.X
        test
            PowerSeries.tan
            (PowerSeries.revert (PowerSeries.integral (1N / (1N + x ** 2))))

    /// https://www.emathzone.com/tutorials/calculus/maclaurin-series-of-sqrt1x.html
    [<TestMethod>]
    member __.Sqrt() =
        let x = PowerSeries<BigRational>.X
        let expected = [1N; 1N/2N; -1N/8N; 1N/16N]
        Assert.AreEqual(
            expected,
            (1N + x)
                |> PowerSeries.sqrt
                |> PowerSeries.take expected.Length)
