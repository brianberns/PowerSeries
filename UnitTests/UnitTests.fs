﻿namespace PowerSeries

open System
open System.Numerics

open Microsoft.VisualStudio.TestTools.UnitTesting

open MathNet.Numerics

[<TestClass>]
type UnitTest() =

    member inline __.TypeTest<'T
                        when ^T : (static member Zero : ^T)
                        and ^T : (static member One : ^T)
                        and ^T : (static member (+) : ^T * ^T -> ^T)
                        and ^T : (static member (*) : ^T * ^T -> ^T)
                        and ^T : (static member (/) : ^T * ^T -> ^T)
                        and ^T : (static member (~-) : ^T -> ^T)
                        and ^T : equality>(cast : int -> 'T) =

        let coeff = [1; 0; -2]
        let expected = [1; 0; -6; 0; 12; 0; -8; 0; 0; 0]

        let series =
            coeff
                |> List.map cast
                |> PowerSeries.OfList
        Assert.AreEqual(
            expected |> List.map cast,
            (series ** 3).Take(expected.Length))

    [<TestMethod>]
    member this.Types() =
        this.TypeTest(id)
        this.TypeTest(int64)
        this.TypeTest(BigInteger)
        this.TypeTest(decimal)
        this.TypeTest(BigRational.FromInt)

    [<TestMethod>]
    member __.Expression() =

        let x = PowerSeries<int>.X
        let series = (1 - 2*x**2) ** 3
        Assert.AreEqual(
            [1; 0; -6; 0; 12; 0; -8; 0; 0; 0],
            series.Take(10))

        let series = 1 / (1-x)
        Assert.AreEqual(
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 1],
            series.Take(10))

        let series = 1 / (1-x)**2
        Assert.AreEqual(
            [1; 2; 3; 4; 5; 6; 7; 8; 9; 10],
            series.Take(10))

    [<TestMethod>]
    member __.Calculus() =
 
        let x = PowerSeries<int>.X
        let series = x * x |> PowerSeries.deriv
        Assert.AreEqual(
            [0; 2; 0],
            series.Take(3))

        let series = series |> PowerSeries.integral
        Assert.AreEqual(
            [0; 0; 1],
            series.Take(3))

    [<TestMethod>]
    member __.Exp() =
        let expected = [1N; 1N; 1N/2N; 1N/6N; 1N/24N; 1N/120N; 1N/720N]
        Assert.AreEqual(
            expected,
            PowerSeries<BigRational>.Exp.Take(expected.Length))
        Assert.AreEqual(
            Math.E,
            PowerSeries<float>.Exp
                |> PowerSeries.eval 100 1.0)
        Assert.AreEqual(
            Math.E,
            PowerSeries<BigRational>.Exp
                |> PowerSeries.eval 100 1N
                |> float)

    [<TestMethod>]
    member __.Trig() =

        let test (seriesA : PowerSeries<_>) (seriesB : PowerSeries<_>) =
            Assert.AreEqual(
                seriesA.Take(30),
                seriesB.Take(30))

        test
            PowerSeries.sin
            (sqrt (1N - PowerSeries.cos ** 2))

        let x = PowerSeries<BigRational>.X
        test
            PowerSeries.tan
            (PowerSeries.revert (PowerSeries.integral (1N / (1N + x ** 2))))

    /// https://www.emathzone.com/tutorials/calculus/maclaurin-series-of-sqrt1x.html
    [<TestMethod>]
    member __.Sqrt() =
        let sqrt1PlusX =
            sqrt (1N + PowerSeries<BigRational>.X)
        let expected = [1N; 1N/2N; -1N/8N; 1N/16N]
        Assert.AreEqual(
            expected,
            sqrt1PlusX.Take(expected.Length))
        Assert.AreEqual(
            sqrt 2.0,
            sqrt1PlusX
                |> PowerSeries.eval 200 1N
                |> float,
            0.0001)
