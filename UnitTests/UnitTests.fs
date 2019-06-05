namespace Bernsrite.PowerSeries

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type UnitTest() =

    [<TestMethod>]
    member __.Test1() =
        printfn "%s" (PowerSeries.zero |> PowerSeries.toString)
