# Power series, power serious

A power series is characterized by an infinite list of coefficients: a<sub>0</sub> + a<sub>1</sub>*x + a<sub>2</sub>*x<sup>2</sup> + a<sub>3</sub>*x<sup>3</sup> + ...

This F# class library models these coefficients as an infinite, lazy list. The idea comes from a functional pearl by M. Douglas McIlroy called [Power Series, Power Serious](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.333.3156&rep=rep1&type=pdf).

## Implementing infinite lists in F#

F# already has the concept of an infinite collection in the `Seq` type. However, this type doesn't support recurisive logic like the `List` type does. Haskell's lists are inherently lazy, but we have to implement that sort of logic manually in F#:

```F#
type InfiniteLazyList<'T> =
    | (::) of ('T * Lazy<InfiniteLazyList<'T>>)
```

But wait, how do we instantiate such a list if it is inherently infinite in length? The answer is a recursive value:

```F#
let rec ones = 1 :: lazy ones   // generates warning FS0040: "This and other recursive references to the object(s) being defined will be checked for initialization-soundness at runtime through the use of a delayed reference."
```

This technique pushes F# to its limits in some ways (hence the compiler warning), but can successfully represent power series in F#.
