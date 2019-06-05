namespace Bernsrite.PowerSeries

module List =

    let (|Cons|Nil|) = function
        | [] -> Nil
        | head :: tail -> Cons(head, tail)

type PowerSeries<'T> =
    | (::) of ('T * Lazy<PowerSeries<'T>>)

module PowerSeries =

    let inline toString series =
        let rec loop level series =
            match series with
                | f :: _ when level = 0 -> sprintf "%A, ..." f
                | f :: fs -> sprintf "%A, %s" f (fs.Value |> loop (level - 1))
        series |> loop 3

    let rec take n (f :: fs) =
        if n <= 0 then
            []
        else
            List.Cons(f, fs.Value |> take (n-1))

    let rec zero =
        0 :: lazy zero

    let ofInt n =
        n :: lazy zero

    let rec ofList ns =
        match ns with
            | List.Nil -> zero
            | List.Cons (head, tail) -> head :: lazy (ofList tail)

    let rec x =
        0 :: lazy (1 :: lazy zero)

    let rec scale c (f :: fs) =
        c * f :: lazy (scale c fs.Value)

    let rec add (f :: fs) (g :: gs) =
        (f + g) :: lazy (add fs.Value gs.Value)

    let rec mult (f :: fs) (g :: gs) =
        f * g :: lazy (add (scale f gs.Value) (mult fs.Value (g :: gs)))

type PowerSeries<'T> with

    static member inline Zero =
        PowerSeries.zero

    static member inline (.*)(c, series) =
        PowerSeries.scale c series

    static member inline (+)(seriesA, seriesB) =
        PowerSeries.add seriesA seriesB

    static member inline (*)(seriesA, seriesB) =
        PowerSeries.mult seriesA seriesB
