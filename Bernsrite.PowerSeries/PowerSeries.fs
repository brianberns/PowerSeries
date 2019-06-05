namespace Bernsrite.PowerSeries

open System
open Microsoft.FSharp.Core.LanguagePrimitives

module List =

    let (|Cons|Nil|) = function
        | [] -> Nil
        | head :: tail -> Cons(head, tail)

type PowerSeries<'T
        when ^T : (static member Zero : ^T)
        and ^T : (static member One : ^T)
        and ^T : (static member (+) : ^T * ^T -> ^T)
        and ^T : (static member (*) : ^T * ^T -> ^T)> =
    | (::) of ('T * Lazy<PowerSeries<'T>>)

/// Inspired by "Power Series, Power Serious" by M. Douglas McIlroy
/// http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.333.3156&rep=rep1&type=pdf
module PowerSeries =

    let inline toString series =
        let rec loop level = function
            | f :: _ when level = 0 ->
                sprintf "%A, ..." f
            | f :: fs ->
                sprintf "%A, %s" f (fs.Value |> loop (level - 1))
        series |> loop 3

    let inline take<'T
            when ^T : (static member Zero : ^T)
            and ^T : (static member One : ^T)
            and ^T : (static member (+) : ^T * ^T -> ^T)
            and ^T : (static member (*) : ^T * ^T -> ^T)> n series =
        let rec loop n (f : 'T :: fs) =
            if n <= 0 then
                []
            else
                List.Cons(f, fs.Value |> loop (n-1))
        loop n series

    let inline zero<'T
            when ^T : (static member Zero : ^T)
            and ^T : (static member One : ^T)
            and ^T : (static member (+) : ^T * ^T -> ^T)
            and ^T : (static member (*) : ^T * ^T -> ^T)> =
        let rec value =  GenericZero<'T> :: lazy value
        value

    let inline one<'T
            when ^T : (static member Zero : ^T)
            and ^T : (static member One : ^T)
            and ^T : (static member (+) : ^T * ^T -> ^T)
            and ^T : (static member (*) : ^T * ^T -> ^T)> =
        let rec value = GenericOne<'T> :: lazy zero
        value

    let inline ofList<'T
            when ^T : (static member Zero : ^T)
            and ^T : (static member One : ^T)
            and ^T : (static member (+) : ^T * ^T -> ^T)
            and ^T : (static member (*) : ^T * ^T -> ^T)> (ns : List<'T>) =
        let rec loop = function
            | List.Nil -> zero
            | List.Cons (head, tail) -> head :: lazy (loop tail)
        ns |> loop

    let inline x<'T
            when ^T : (static member Zero : ^T)
            and ^T : (static member One : ^T)
            and ^T : (static member (+) : ^T * ^T -> ^T)
            and ^T : (static member (*) : ^T * ^T -> ^T)> =
        let rec value = GenericZero<'T> :: lazy (GenericOne<'T> :: lazy zero)
        value

    let inline negate series =
        let rec loop = function
            | f :: fs -> -f :: lazy (loop fs.Value)
        series |> loop

    let inline scale (c : 'T) series =
        let rec loop (f :: fs) =
            (c * f) :: lazy (fs.Value |> loop)
        series |> loop

    let inline add seriesF seriesG =
        let rec loop (f : 'T :: fs) (g : 'T :: gs) =
            (f + g) :: lazy (loop fs.Value gs.Value)
        loop seriesF seriesG

    let inline sub seriesF seriesG =
        add seriesF (negate seriesG)

    let inline mult seriesF seriesG =
        let rec loop (f : 'T :: fs) (g : 'T :: gs) =
            (f * g) :: lazy (add (scale f gs.Value) (loop fs.Value (g :: gs)))
        loop seriesF seriesG

    let inline div seriesF seriesG =
        let rec loop (f : 'T :: fs) (g : 'T :: gs) =
            if f = GenericZero<'T> && g = GenericZero<'T> then
                loop fs.Value gs.Value
            else
                let q = f / g
                q :: lazy (loop (sub fs.Value (scale q gs.Value)) (g :: gs))
        loop seriesF seriesG

    let inline pow n series =
        let rec loop n series =
            match n with
                | 0 -> one
                | n when n > 0 ->
                    series
                        |> loop (n - 1)
                        |> mult series
                | _ -> raise <| NotSupportedException()
        series |> loop n

    let inline compose seriesF seriesG =
        let rec loop (f : 'T :: fs) (g : 'T :: gs) =
            if g = GenericZero<'T> then
                f :: lazy (mult gs.Value (loop fs.Value (g :: gs)))
            else
                raise <| NotSupportedException()
        loop seriesF seriesG
    
    let inline revert series =
        let rec loop (f : 'T :: fs) =
            if f = GenericZero<'T> then
                let rec rs =
                    GenericZero<'T> :: lazy (div one (compose fs.Value rs))
                rs
            else
                raise <| NotSupportedException()
        loop series

    let inline deriv (_ :: fs) =
        let rec deriv1 (g : 'T :: gs) n =
            (n * g) :: lazy (deriv1 gs.Value (n + GenericOne<'T>))
        deriv1 fs.Value GenericOne<'T>

    let inline integral fs =
        let rec int1 (g : 'T :: gs) n : PowerSeries<'T> =
            (g / n) :: lazy (int1 gs.Value (n + GenericOne<'T>))
        GenericZero<'T> :: lazy (int1 fs GenericOne<'T>)

type PowerSeries<'T
        when ^T : (static member Zero : ^T)
        and ^T : (static member One : ^T)
        and ^T : (static member (+) : ^T * ^T -> ^T)
        and ^T : (static member (*) : ^T * ^T -> ^T)> with

    static member inline Zero =
        PowerSeries.zero<'T>

    static member inline One =
        PowerSeries.one<'T>

    static member inline (~-) series =
        PowerSeries.negate series

    static member inline (+)(seriesF, seriesG) =
        PowerSeries.add seriesF seriesG

    static member inline (-)(seriesF, seriesG) =
        PowerSeries.sub seriesF seriesG

    static member inline (.*)(c, series) =
        PowerSeries.scale c series

    static member inline (*)(seriesF, seriesG) =
        PowerSeries.mult seriesF seriesG

    static member inline (/)(seriesF, seriesG) =
        PowerSeries.div seriesF seriesG

    static member inline Pow(series, n) =
        PowerSeries.pow n series

module NumericLiteralG =
    let FromZero () = PowerSeries.zero<int>
    let FromOne () = PowerSeries.one<int>
    let FromInt32 (n : int) = PowerSeries.ofList [n]
    let FromInt64 (n : int64) = PowerSeries.ofList [n]
