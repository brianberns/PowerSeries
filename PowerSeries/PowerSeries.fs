(*
 * Inspired by "Power Series, Power Serious" by M. Douglas McIlroy
 * http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.333.3156&rep=rep1&type=pdf
 *)

namespace PowerSeries

open System
open Microsoft.FSharp.Core.LanguagePrimitives
open MathNet.Numerics

module List =

    /// Aliases for list constructors, so we can still access them after overriding their
    /// usual names.
    let (|Cons|Nil|) = function
        | [] -> Nil
        | head :: tail -> Cons(head, tail)

#nowarn "40"   // recursive values

/// A generic number.
type Numeric<'T
    when 'T : (static member Zero : 'T)
    and 'T : (static member One : 'T)
    and 'T : (static member (+) : 'T * 'T -> 'T)
    and 'T : (static member (*) : 'T * 'T -> 'T)
    and 'T : (static member (/) : 'T * 'T -> 'T)
    and 'T : (static member (~-) : 'T -> 'T)
    and 'T : equality> = 'T

/// A power series: a0 + a1*x + a2*x^2 + a3*x^3 + ...
type PowerSeries<'T when Numeric<'T>> =
    | (::) of 'T * Lazy<PowerSeries<'T>>

    /// Power series for 0 = 0 + 0*x + 0*x^2 + ...
    static member inline Zero =
        let rec value = GenericZero<'T> :: lazy value
        value

    /// Power series for a constant = a0 + 0*x + 0*x^2 + ...
    static member inline Constant(c) =
        c :: lazy PowerSeries<'T>.Zero

    /// Power series for 1 = 1 + 0*x + 0*x^2 + ...
    static member inline One =
        PowerSeries.Constant(GenericOne<'T>)

    /// Power series for a variable x = 0 + 1*x.
    static member inline X =
        GenericZero<'T> :: lazy PowerSeries.One

    /// Constructs a power series from the given coeffecients.
    static member inline OfList(ns : List<'T>) =
        let rec loop = function
            | List.Nil -> PowerSeries.Zero
            | List.Cons (head, tail) -> head :: lazy (loop tail)
        ns |> loop

    /// Negates the given power series.
    static member inline (~-)(series) =
        let rec loop (f : 'T :: fs) =
            -f :: lazy (loop fs.Value)
        loop series

    /// Adds the given power series.
    static member inline (+)(seriesF, seriesG) =
        let rec loop (f :: fs) (g :: gs) =
            (f + g) :: lazy (loop fs.Value gs.Value)
        loop seriesF seriesG

    /// Adds the given constant value to the given power series.
    static member inline (+)(value : 'T, series) =
        PowerSeries.Constant(value) + series

    /// Subtracts the given power series.
    static member inline (-)(seriesF : PowerSeries<'T>, seriesG) =
        seriesF + (-seriesG)

    /// Subtracts the given power series from the given constant value.
    static member inline (-)(value : 'T, series) =
        PowerSeries.Constant(value) - series

    /// Multiplies the given power series by a constant.
    static member inline (*)(c : 'T, series) =
        let rec loop (f :: fs) =
            (c * f) :: lazy (loop fs.Value)
        loop series

    /// Multiplies the given power series.
    static member inline (*)(seriesF, seriesG) =
        let (.*) (a : 'T) (b : PowerSeries<'T>) =   // F# compiler workaround
            PowerSeries<'T>.(*)(a, b)
        let rec loop (f : 'T :: fs) (g :: gs) =
            (f * g) :: lazy (f .* gs.Value + loop fs.Value (g :: gs))
        loop seriesF seriesG

    /// Divides the given power series.
    static member inline (/)(seriesF, seriesG) =
        let (.*) (a : 'T) (b : PowerSeries<'T>) =   // F# compiler workaround
            PowerSeries<'T>.(*)(a, b)
        let rec loop (f :: fs) (g :: gs) =
            if f = GenericZero<'T> && g = GenericZero<'T> then
                loop fs.Value gs.Value
            else
                let q = f / g
                q :: lazy (loop (fs.Value - q .* gs.Value) (g :: gs))
        loop seriesF seriesG

    /// Divides the given constant value by the given power series.
    static member inline (/)(value : 'T, series) =
        PowerSeries.Constant(value) / series

    /// Raises the given power series to a power.
    static member inline Pow(series : PowerSeries<'T>, n) =
        pown series n

    /// Takes a finite number of coeffecients from the given power series.
    member inline series.Take(n) =
        let rec loop n (f :: fs) =
            if n <= 0 then
                []
            else
                List.Cons(f, fs.Value |> loop (n-1))
        loop n series

    /// Display string.
    member inline series.String =
        series.Take(3)
            |> sprintf "%A, ..."

module PowerSeries =

    /// Composes two power series: F(G).
    let inline compose seriesF seriesG =
        let rec loop (f :: fs) (g :: gs) =
            if g = GenericZero then
                f :: lazy (gs.Value * loop fs.Value (g :: gs))
            else
                raise <| NotSupportedException()
        loop seriesF seriesG
    
    /// Reverts the given power series. (Finds its inverse.)
    let inline revert series =
        let rec loop (f :: fs) =
            if f = GenericZero then
                let rec rs =
                    GenericZero :: lazy (PowerSeries.One / compose fs.Value rs)
                rs
            else
                raise <| NotSupportedException()
        loop series

    /// Answers the derivative of the given power series.
    let inline deriv (_ :: fs) =
        let rec deriv1 (g : 'T :: gs) n =
            (n * g) :: lazy (deriv1 gs.Value (n + GenericOne<'T>))
        deriv1 fs.Value GenericOne<'T>

    /// Answers the integral of the given power series.
    let inline internal lazyIntegral (fs : Lazy<_>) =
        let rec int1 (g : 'T :: gs) n =
            (g / n) :: lazy (int1 gs.Value (n + GenericOne<'T>))
        GenericZero<'T> :: lazy (int1 fs.Value GenericOne<'T>)

    /// Answers the integral of the given power series.
    let inline integral series =
        lazyIntegral (lazy series)

    /// Evaluates the given series for the given value, using the given
    /// number of terms.
    let inline eval n (x : 'T) series =
        let rec loop n (f : 'T :: fs) =
            if n <= 0 then
                GenericZero<'T>
            else
                f + (x * fs.Value |> loop (n - 1))
        loop n series

    /// Sine and cosine functions.
    let sin, cos =
        let rec lazySin =
            lazy (lazyIntegral lazyCos)
        and lazyCos =
            lazy (PowerSeries<BigRational>.One - (lazyIntegral lazySin))
        lazySin.Value, lazyCos.Value

    /// Tangent function.
    let tan = sin / cos

/// Power series extensions.
type PowerSeries<'T when Numeric<'T>> with

    /// Exponential function.
    static member inline Exp =
        let rec lazyExp =
            lazy (PowerSeries<'T>.One + (PowerSeries.lazyIntegral lazyExp))
        lazyExp.Value

    /// Answers the square root of the given series.
    static member inline Sqrt(series) =
        let fail () = failwith "Can't compute square root"
        let rec loop (f : 'T :: fs) =
            if f = GenericZero<'T> then
                let (f :: fs) = fs.Value
                if f = GenericZero<'T> then
                    f :: lazy (loop fs.Value)
                else fail ()
            elif f = GenericOne<'T> then
                let rec lazyQs : Lazy<PowerSeries<'T>> =
                    let lazyDiv =
                        let num = PowerSeries.deriv (GenericOne<'T> :: fs)
                        let lazyDen =
                            lazy (lazyQs.Value + lazyQs.Value)
                        lazy (num / lazyDen.Value)
                    lazy (PowerSeries<'T>.One + PowerSeries.lazyIntegral lazyDiv)
                lazyQs.Value
            else fail ()
        series |> loop
