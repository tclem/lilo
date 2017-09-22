### Hindley-Milner Type Inference

*8/28/2017*

Took a first pass at Hindley-Milner type inference, largely by following along with [Stephen Diehl's chapter on the subject](http://dev.stephendiehl.com/fun/006_hindley_milner.html). I made a couple of minor modification to the language syntax, removing the need to specify type annotations and adding in the concept of a type variable. I also introduced `Scheme` as a way to model polymorphic types:

``` haskell
type TName = String

data Type
  = TVar TName
  | TInt
  | TBool
  | TArr Type Type
  deriving(Eq, Ord, Show)

data Scheme = Forall [Name] Type
  deriving(Eq, Show)
```

The implementation path I took is very much a direct derivation of [Algorithm W Step by Step](http://catamorph.de/documents/AlgorithmW.pdf), which is recommended as background reading.

Here's some basic inference in action:

```sh
# Our friend the identity function, now polymorphic.
λ infer (parseExpr "(\\x. x)")
Right (Forall ["a"] (TArr (TVar "a") (TVar "a")))
λ infer (parseExpr "(\\x. x) 1")
Right (Forall [] TInt)
λ infer (parseExpr "(\\x. x) true")
Right (Forall [] TBool)

# The ⍵-combinator get's caught in the type system (The initial untyped lambda calculus would have allowed this).
λ infer (parseExpr "(\\x. x x)")
Left "cannot construct the infinite type a = (a -> b)"

# Free variables not allowed.
λ infer (parseExpr "(\\x. y)")
Left "free variable y"

# Types have to line up!
λ infer (parseExpr "(\\x. (\\y . x y)) 1")
Left "failed to unify (c -> d) with Int"
```
