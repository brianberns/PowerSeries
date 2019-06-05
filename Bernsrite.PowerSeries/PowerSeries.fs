namespace Bernsrite.PowerSeries

type PowerSeries<'T when ^T : (static member (~-) : ^T -> ^T)> =
    | (::) of ('T * Lazy<PowerSeries<'T>>)

module PowerSeries =

    let rec zero =
        0 :: lazy zero

    let ofInt n =
        n :: lazy zero

    let rec x =
        0 :: lazy (1 :: lazy zero)

    let rec negate = function
        | f :: fs -> -f :: lazy (negate fs.Value)

    let inline toString<'T when ^T : (static member (~-) : ^T -> ^T)> (series : PowerSeries<'T>) =
        let rec loop level series =
            match series with
                | f :: _ when level = 0 -> sprintf "%A, ..." f
                | f :: fs -> sprintf "%A, %s" f (fs.Value |> loop (level - 1))
        series |> loop 3

type PowerSeries<'T when ^T : (static member (~-) : ^T -> ^T)> with

    static member inline Zero =
        PowerSeries.zero

    static member inline (~-) series =
        PowerSeries.negate series

    static member inline (+)(seriesA, seriesB) =
        ()
