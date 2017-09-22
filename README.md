lilo is a series of simple, lambda calculi implemented in Haskell.

I'm currently experimenting with defining the syntax expressions of the language using the techniques described in [Data types à la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf).


## Usage:

``` haskell
> eval $ parseExpr "(\\x -> x) 1"
```

## Learning log

I'm tracking my progress in a series of small, informal posts:

- [A lambda calculus is born](docs/2017-08-18-a-lambda-calculus-is-born.md)
- [Implementing pairs](docs/2017-08-21-implementing-pairs.md)
- [Sum types](docs/2017-08-22-sum-types.md)
- [Hindley-Milner type inference](docs/2017-08-28-hindley-milner-type-inference.md)
- [Syntax à la carte](docs/2017-09-22-syntax-a-la-carte.md)

## References:

- A lot of the ideas and code here is inspired by and directly from [Stephen Diehl](http://dev.stephendiehl.com/fun).
- [Church booleans](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans)
- [Fixed point combinators](https://en.wikipedia.org/wiki/Fixed-point_combinator)
