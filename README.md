# Power series, power serious

A power series is characterized by an infinite list of coefficients: a<sub>0</sub> + a<sub>1</sub>*x + a<sub>2</sub>*x<sup>2</sup> + a<sub>3</sub>*x<sup>3</sup> + ... . For example, the power seres for `cos x` is 1 - x<sup>2</sup>/2! + x<sup>4</sup>/4! - x<sup>6</sup>/6! + ... and the coefficients for the powers of x in this series are 1, 0, -1/2, 0, 1/24, ...

This F# class library models the coefficients of a power series as an infinite, lazy list. The idea comes from a functional pearl by M. Douglas McIlroy called [Power Series, Power Serious](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.333.3156&rep=rep1&type=pdf).

## Implementing infinite lists in F#

F# already has the concept of an infinite collection in the `Seq` type. However, this type doesn't support recursive logic like the `List` type does. Haskell's lists are inherently lazy, but we have to implement this behavior manually in F#:

```F#
type InfiniteLazyList<'T> =
    | (::) of ('T * Lazy<InfiniteLazyList<'T>>)
```

But wait, how do we instantiate such a list if it is inherently infinite in length? The answer is a recursive value:

```F#
let rec ones = 1 :: lazy ones   // generates warning FS0040: "This and other recursive references to the object(s) being defined will be checked for initialization-soundness at runtime through the use of a delayed reference."
```

This technique pushes F# to its limits in some ways (hence the compiler warning), but can successfully represent power series in F#.

## Example series

A power series whose coefficients are all zero has the value zero (i.e. `0 + 0*x + 0*x**2 + 0*x**3 + ... = 0 + 0 + 0 + ... = 0`). We implement this series using `GenericZero` so the actual type of the coefficients can be `int`, `BigRational` (from `System.Numerics`), or any other numeric type that we choose.

```F#
let rec zero = GenericZero<'T> :: lazy zero
```

Similarly, we can represent the term `x` (i.e. `0 + 1*x`) with the coefficient 0, followed by 1, followed by an infinite list of zeros:

```F#
let rec x = GenericZero<'T> :: lazy (GenericOne<'T> :: lazy zero)
```

We can then construct power series algebraically by implementing basic arithmetic operations on power series. For example, the following expression uses subtraction, multiplication, and exponentiation to construct a power series:

```F#
let series = (1 - 2*x**2) ** 3   // first ten coefficients are 1, 0, -6, 0, 12, 0, -8, 0, 0, 0
```

## Power serious

With that foundation in place, we can implement some powerful behavior, such as integration of power series:

```F#
let lazyIntegral (fs : Lazy<_>) =
    let rec int1 (g : 'T :: gs) n : PowerSeries<'T> =
        (g / n) :: lazy (int1 gs.Value (n + GenericOne<'T>))
    GenericZero<'T> :: lazy (int1 fs.Value GenericOne<'T>)
    
/// Answers the integral of the given power series.
let integral series =
    lazyIntegral (lazy series)
```

Note that `lazyIntegral` generates a zero before it attempts to evaluate its argument. This allows for self-referential usages that are close to magical, such as the exponential function, `exp x`:

```F#
/// Exponential function: exp = 1 + (integral exp)
let exp =
    let rec lazyExp =
        lazy (PowerSeries<BigRational>.One + (lazyIntegral lazyExp))
    lazyExp.Value
```

The coefficients of this series are rational numbers: 1, 1, 1/2, 1/6, 1/24, 1/120, 1/720, ... . We can then take, say, the first 100 coefficients, evaluate them for x = 1, add them together, and convert the resulting sum to a `float`, producing a value that matches `Math.E` exactly:

```F#
Assert.AreEqual(
        Math.E,
        PowerSeries.exp
            |> PowerSeries.eval 100 1N
            |> float)
```

## Usage

The power series type is `PowerSeries<'T>`, with a corresponding module of functions that's also called `PowerSeries`. Working examples, including square roots, trigonometry, and calculus can be found in the [unit tests](https://github.com/brianberns/Bernsrite.PowerSeries/blob/master/UnitTests/UnitTests.fs).
