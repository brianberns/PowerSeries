# Power series, power serious in F#

A power series is characterized by an infinite list of coefficients of powers of a variable, `x`:

```
a₀ + a₁x + a₂x² + a₃x³ + ...
```

For example, the power series for `cos x` is:

```
  1 - x²/2! + x⁴/4! - x⁶/6! ...
= 1 - x²/2 + x⁴/24 - x⁶/720 ...
= 1 + (0)x + (-1/2)x² + (0)x³ + (1/24)x⁴ + (0)x⁵ + (-1/720)x⁶ ...
```

As you can see, the coefficients for the powers of `x` in this series are `1`, `0`, `-1/2`, `0`, `1/24`, `0`, `-1/720`, ...

Power series can be used to compute a remarkable variety of expressions. This F# class library models the coefficients of a power series as an infinite, lazy list. The idea comes from a functional pearl by M. Douglas McIlroy called [Power Series, Power Serious](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.333.3156&rep=rep1&type=pdf).

## Implementing infinite lists in F#

In order to work with power series, we need a collection type that is both infinite and recursive. F# sequences are lazy (and thus potentially infinite) and F# lists are recursive, but F# doesn't have a built-in type that supports both behaviors. So, to get the necessary combination in F#, we have to implement it manually:

```fsharp
type InfiniteLazyList<'T> =
    | (::) of ('T * Lazy<InfiniteLazyList<'T>>)
```

This defines an infinite lazy list as a "head" element connected to an infinite lazy "tail" via the `::` operator. But how do we instantiate such a list if it is inherently infinite in length? The answer is a recursive value. For example, this is a list consisting of the string `"hello"` repeating infinitely: 

```fsharp
// "hello", "hello", "hello", ...
let rec hellos = "hello" :: lazy hellos   // generates warning FS0040: "This and other recursive references to the object(s) being defined will be checked for initialization-soundness at runtime through the use of a delayed reference."
```

This technique pushes F# to its limits in some ways (hence the compiler warning), but can successfully represent power series in F#.

## Example series

A power series whose coefficients are all zero has the value zero (i.e. `0 + 0x + 0x² + 0x³ + ... = 0 + 0 + 0 + ... = 0`). We implement this series as an infinite list of `GenericZero`s. The actual type of the coefficients can thus be `int`, `BigRational` (from `System.Numerics`), or any other numeric type that we choose.

```fsharp
// 0, 0, 0, ... = 0
let rec zero = GenericZero<'T> :: lazy zero
```

In the same way, we can represent any constant (e.g. `0`, `1`, `2`, ...) as a power series. For example, the number one is defined as this series:

```fsharp
// 1, 0, 0, ... = 1
let rec one = GenericOne<'T> :: lazy zero
```

Similarly, we represent the term `x` (i.e. `0 + 1x`) as the coefficient `0`, followed by `1`, followed by an infinite list of zeros:

```fsharp
// 0, 1, 0, 0, 0, ... = 0 + 1x = x
let x = GenericZero<'T> :: lazy (GenericOne<'T> :: lazy zero)
```


## Power serious

The actual `PowerSeries` type is defined just like `InfiniteLazyList`, but with elements constrained to be numeric (which is painful in F#, but still doable):

```fsharp
/// A power series: a0 + a1*x + a2*x^2 + a3*x^3 + ...
type PowerSeries<'T
        when ^T : (static member Zero : ^T)
        and ^T : (static member One : ^T)
        and ^T : (static member (+) : ^T * ^T -> ^T)
        and ^T : (static member (*) : ^T * ^T -> ^T)
        and ^T : (static member (/) : ^T * ^T -> ^T)
        and ^T : (static member (~-) : ^T -> ^T)
        and ^T : equality> =
    | (::) of ('T * Lazy<PowerSeries<'T>>)
```

Addition of power series is performed by adding corresponding coefficients together recursively:

```fsharp
/// Adds the given power series.
let inline add seriesF seriesG =
    let rec loop (f : 'T :: fs) (g : 'T :: gs) =
        (f + g) :: lazy (loop fs.Value gs.Value)
    loop seriesF seriesG
```

Subtraction, multiplication, and exponentiation of power series are defined similarly. (See McIlroy's paper for the math, which is fairly straightforwad.) With those operations in place, we can construct power series algebraically. For example, the following expression represents the arbitrary polynomial `(1 - 2x²)³`:

```fsharp
// (1 - 2x²)³ = 1 - 6x² + 12x⁴ - 8x⁶
let polynomial = (1 - 2*x**2) ** 3   // coefficients are 1, 0, -6, 0, 12, 0, -8, 0, 0, 0, ...
```

With that foundation in place, we can implement even more sophisticated behavior, such as derivatives and integrals of power series:

```fsharp
let lazyIntegral (fs : Lazy<_>) =
    let rec int1 (g : 'T :: gs) n : PowerSeries<'T> =
        (g / n) :: lazy (int1 gs.Value (n + GenericOne<'T>))
    GenericZero<'T> :: lazy (int1 fs.Value GenericOne<'T>)
    
/// Answers the integral of the given power series.
let integral series =
    lazyIntegral (lazy series)
```

Note that `lazyIntegral` generates a zero before it attempts to evaluate its argument. This allows for self-referential usages that are close to magical, such as the exponential function, `exp x`:

```fsharp
/// Exponential function: exp = 1 + (integral exp)
let exp =
    let rec lazyExp =
        lazy (PowerSeries<BigRational>.One + (lazyIntegral lazyExp))
    lazyExp.Value
```

The coefficients of this series are rational numbers: `1`, `1`, `1/2`, `1/6`, `1/24`, `1/120`, `1/720`, ... . We can then take, say, the first 100 coefficients, evaluate them for `x = 1`, add them together, and convert the resulting sum to a `float`, producing a value that matches `Math.E` exactly:

```fsharp
Assert.AreEqual(
    Math.E,
    PowerSeries.exp
        |> PowerSeries.eval 100 1N
        |> float)
```

## Usage

Many such computations are possible using this library. Working examples, including square roots, trigonometry, and calculus can be found in the [unit tests](https://github.com/brianberns/Bernsrite.PowerSeries/blob/master/UnitTests/UnitTests.fs).
