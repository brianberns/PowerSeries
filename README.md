# Power series, power serious

A power series is characterized by an infinite list of coefficients: a<sub>0</sub> + a<sub>1</sub>*x + a<sub>2</sub>*x<sup>2</sup> + a<sub>3</sub>*x<sup>3</sup> + ...

For example, the power seres for cos x is 1 - x<sup>2</sup>/2! + x<sup>4</sup>/4! - x<sup>6</sup>/6! + ... and the coefficients for the powers of x in this series are 1, 0, -1/2, 0, 1/24, ...

This F# class library models the coefficients of a power series as an infinite, lazy list. The idea comes from a functional pearl by M. Douglas McIlroy called [Power Series, Power Serious](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.333.3156&rep=rep1&type=pdf).

## Implementing infinite lists in F#

F# already has the concept of an infinite collection in the `Seq` type. However, this type doesn't support recurisive logic like the `List` type does. Haskell's lists are inherently lazy, but we have to implement this behavior manually in F#:

```F#
type InfiniteLazyList<'T> =
    | (::) of ('T * Lazy<InfiniteLazyList<'T>>)
```

But wait, how do we instantiate such a list if it is inherently infinite in length? The answer is a recursive value:

```F#
let rec ones = 1 :: lazy ones   // generates warning FS0040: "This and other recursive references to the object(s) being defined will be checked for initialization-soundness at runtime through the use of a delayed reference."
```

This technique pushes F# to its limits in some ways (hence the compiler warning), but can successfully represent power series in F#.

## Examples

A power series whose coefficients are all zero has the value zero. We implement this using `GenericZero` so the actual type of the coefficients can be `int`, `BigRational` (from `System.Numerics`), or any other numeric type that we choose.

```F#
let rec zero = GenericZero<'T> :: lazy zero
```

Similarly, we can represent the term `x` (i.e. `0 + 1 * x`) with the coefficient 0, followed by 1, followed by an infinite list of zeros:

```F#
let rec x = GenericZero<'T> :: lazy (GenericOne<'T> :: lazy zero)
```
