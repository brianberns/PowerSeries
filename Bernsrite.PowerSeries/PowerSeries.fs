/// Inspired by "Power Series, Power Serious" by M. Douglas McIlroy
/// http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.333.3156&rep=rep1&type=pdf
namespace Bernsrite.PowerSeries

open System
open Microsoft.FSharp.Core.LanguagePrimitives
open MathNet.Numerics

module List =

    /// Aliases for list constructors, so we can still access them after overriding their
    /// usual names.
    let (|Cons|Nil|) = function
        | [] -> Nil
        | head :: tail -> Cons(head, tail)

/// A power series: a0 + a1*x + a2*x^2 + a3*x^3 + ...
type PowerSeries<'T
        when ^T : (static member Zero : ^T)
        and ^T : (static member One : ^T)
        and ^T : (static member (+) : ^T * ^T -> ^T)
        and ^T : (static member (*) : ^T * ^T -> ^T)> =
    | (::) of ('T * Lazy<PowerSeries<'T>>)

module internal Internal =

    let inline zero<'T
            when ^T : (static member Zero : ^T)
            and ^T : (static member One : ^T)
            and ^T : (static member (+) : ^T * ^T -> ^T)
            and ^T : (static member (*) : ^T * ^T -> ^T)> =
        let rec value =  GenericZero<'T> :: lazy value
        value

    let inline constant<'T
                when ^T : (static member Zero : ^T)
                and ^T : (static member One : ^T)
                and ^T : (static member (+) : ^T * ^T -> ^T)
                and ^T : (static member (*) : ^T * ^T -> ^T)> (value : 'T) =
        value :: lazy zero

    let inline one<'T
            when ^T : (static member Zero : ^T)
            and ^T : (static member One : ^T)
            and ^T : (static member (+) : ^T * ^T -> ^T)
            and ^T : (static member (*) : ^T * ^T -> ^T)> =
        constant GenericOne<'T>

    let inline x<'T
            when ^T : (static member Zero : ^T)
            and ^T : (static member One : ^T)
            and ^T : (static member (+) : ^T * ^T -> ^T)
            and ^T : (static member (*) : ^T * ^T -> ^T)> =
        GenericZero<'T> :: lazy (GenericOne<'T> :: lazy zero)

    let inline ofList<'T
            when ^T : (static member Zero : ^T)
            and ^T : (static member One : ^T)
            and ^T : (static member (+) : ^T * ^T -> ^T)
            and ^T : (static member (*) : ^T * ^T -> ^T)> (ns : List<'T>) =
        let rec loop = function
            | List.Nil -> zero
            | List.Cons (head, tail) -> head :: lazy (loop tail)
        ns |> loop

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

type PowerSeries<'T
        when ^T : (static member Zero : ^T)
        and ^T : (static member One : ^T)
        and ^T : (static member (+) : ^T * ^T -> ^T)
        and ^T : (static member (*) : ^T * ^T -> ^T)> with

    static member inline Zero =
        Internal.zero<'T>

    static member inline One =
        Internal.one<'T>

    static member inline X =
        Internal.x<'T>

    static member inline (~-) series =
        Internal.negate series

    static member inline (+)(seriesF, seriesG) =
        Internal.add seriesF seriesG

    static member inline (-)(seriesF, seriesG) =
        Internal.sub seriesF seriesG

    static member inline (.*)(c, series) =
        Internal.scale c series

    static member inline (*)(seriesF, seriesG) =
        Internal.mult seriesF seriesG

    static member inline (/)(seriesF, seriesG) =
        Internal.div seriesF seriesG

    static member inline Pow(series, n) =
        Internal.pow n series

module PowerSeries =

    let inline ofList ns =
        Internal.ofList ns

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

    let inline compose seriesF seriesG =
        let rec loop (f : 'T :: fs) (g : 'T :: gs) =
            if g = GenericZero<'T> then
                f :: lazy (gs.Value * (loop fs.Value (g :: gs)))
            else
                raise <| NotSupportedException()
        loop seriesF seriesG
    
    let inline revert series =
        let rec loop (f : 'T :: fs) =
            if f = GenericZero<'T> then
                let rec rs =
                    GenericZero<'T> :: lazy (PowerSeries.One / (compose fs.Value rs))
                rs
            else
                raise <| NotSupportedException()
        loop series

    let inline deriv (_ :: fs) =
        let rec deriv1 (g : 'T :: gs) n =
            (n * g) :: lazy (deriv1 gs.Value (n + GenericOne<'T>))
        deriv1 fs.Value GenericOne<'T>

    let inline private lazyIntegral (fs : Lazy<_>) =
        let rec int1 (g : 'T :: gs) n : PowerSeries<'T> =
            (g / n) :: lazy (int1 gs.Value (n + GenericOne<'T>))
        GenericZero<'T> :: lazy (int1 fs.Value GenericOne<'T>)

    let inline integral series =
        lazyIntegral (lazy series)

    let exp =
        let rec lazyExp =
            lazy (PowerSeries<BigRational>.One + (lazyIntegral lazyExp))
        lazyExp.Value

    let sin, cos =
        let rec lazySin =
            lazy (lazyIntegral lazyCos)
        and lazyCos =
            lazy (PowerSeries<BigRational>.One - (lazyIntegral lazySin))
        lazySin.Value, lazyCos.Value

module NumericLiteralG =
    let FromZero () = Internal.zero<int>
    let FromOne () = Internal.one<int>
    let FromInt32 (n : int) = Internal.constant n
    let FromInt64 (n : int64) = Internal.constant n
