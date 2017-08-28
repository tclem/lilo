lilo is a series of simple, lambda calculi implemented in Haskell.

## Usage:

``` haskell
> eval $ parseExpr "(\\x -> x) 1"
```

## Learning log

### 08/18/2017

For the past couple of days, with guidance from @robrix, I set out to implement a small programming language (really a series of lambda calculi) in service of understanding programming language theory basics and to build towards some additional concepts like type checking and type inference that are relevant and interesting in the context of our team's future plans.

My first step was to implement a tiny, untyped lambda calculus with a parser, pretty printer, and interpreter. The scope of the language I built is quite small, but amazingly ends up being turning complete. The syntax definition, in Haskell, looks like this:

``` haskell
type Name = String

data Expr
  = Var Name
  | Lit Lit
  | App Expr Expr
  | Lam Name Expr
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Show)
```

You have two different literals (integers and booleans), variables, lambdas, and (function) application. That's it. Here's a small demonstration of the lambda calculus:

``` sh
# Pretty printing of ASTs
λ ppExpr (Lit (LInt 1))
"1"
λ ppExpr (Lam "f" (Lit (LInt 1)))
"\\f -> 1"

# Parsing
λ parseExpr "(\\x -> x) 1"
App (Lam "x" (Var "x")) (Lit (LInt 1))

# Eval
λ eval $ parseExpr "(\\x -> x) 1"
Literal (LInt 1)

```

I built two different versions of an interpreter, both with strict evaluation semantics. One version does manual substitution of terms, the other uses an environment to lookup variables captured in scope.

The cool thing about even this tiny language is that you can represent all sorts of stuff with just lambda calculus. For example, church booleans:

```sh
λ eval $ parseExpr "(\\a -> (\\b -> a))"
Closure "a" (Lam "b" (Var "a")) []
λ eval $ parseExpr "(\\a -> (\\b -> a)) 1 0"
Literal (LInt 1)
λ eval $ parseExpr "(\\a -> (\\b -> b))"
Closure "a" (Lam "b" (Var "b")) []
λ eval $ parseExpr "(\\a -> (\\b -> b)) 1 0"
Literal (LInt 0)
```

You can encode pairs, list operations (nil, cons, head, tail), succ/pred, division, signed numbers, etc. You can even formally define recursive functions with the Y combinator, though this is the place where it's fun to know about Curry's paradox, which essential tells you that untyped lambda calculus is unsound as a deductive system (i.e. it is inconsistent in mathematical logic).

### Types

Next, I looked at adding a basic type system to this language. My first pass at that involved just three types and only a minor modification to the original syntax (`Lam` now carries `Type`):

``` haskell
type Name = String

data Expr
  = Var Name
  | Lit Lit
  | App Expr Expr
  | Lam Name Type Expr
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Show)

data Type
  = TInt
  | TBool
  | TArr Type Type
  deriving(Eq, Show)
```

`TInt` and `TBool` should be obvious. `TArr` is the type of functions, represented with an arrow `->` in this language. Parsing picks up some minor changes to pick up on type annotations, and the interpreter is basically unchanged.

The point of interest at this point, is the addition of a type checker. (NOTE: I changed `->` to `.` for lambda definition just to not have to deal with parsing `->` that could mean two things).

``` sh
# Our friend, the  identity function for Ints
λ check $ parseExpr "(\\x : Int . x)"
Right (TArr TInt TInt)

# Looks good
λ check $ parseExpr "(\\x : Int . x) 1"
Right TInt

# Catches our type errors!
λ check $ parseExpr "(\\x : Int . x) true"
Left "expected type TInt but got TBool"
```

> Well typed programs cannot "go wrong" - Robin Milner

Now that I've got a type checker, @robrix and I had some fun parameterizing our syntax and aligning type checking and evaluation with the Haskell type system so that the checker annotates as well as type checks and produces a tree that the evaluator consumes.

``` sh
λ check (parseExpr "(\\x : Int . x) 1")
Right (EIn {eout = App (EIn {eout = Lam "x" TInt (EIn {eout = Var "x", eann = TInt}), eann = TArr TInt TInt}) (EIn {eout = Lit (LInt 1), eann = TInt}), eann = TInt})
λ eval <$> check (parseExpr "(\\x : Int . x) 1")
Right (Literal (LInt 1))

# Showing a function type signature
λ eval <$> check (parseExpr "(\\x : (Int -> Int) . x) (\\y : Int . 0) 1")
Right (Literal (LInt 0))
```

Pretty fun!

### 08/21/17

Worked to implement pairs and `fst`/`snd`. I found this relatively straightforward to implement.

``` sh
λ eval <$> check (parseExpr "(1, true)")
Right (VPair (Literal (LInt 1)) (Literal (LBool True)))
λ eval <$> check (parseExpr "fst (1,true)")
Right (Literal (LInt 1))
λ eval <$> check (parseExpr "snd (1,true)")
Right (Literal (LBool True))
```

Pairs are also valid in type signatures:

``` sh
λ eval <$> check (parseExpr "(\\x : (Int, Bool) . x)")
Right (Closure "x" (EIn {eout = Var "x", eann = TPair TInt TBool}) [])
λ eval <$> check (parseExpr "(\\x : (Int, Bool) . x) 1")
Left "expected type TPair TInt TBool but got TInt"
λ eval <$> check (parseExpr "(\\x : (Int, Bool) . x) (1, true)")
Right (VPair (Literal (LInt 1)) (Literal (LBool True)))
λ eval <$> check (parseExpr "(\\x : (Int, Bool) . fst x) (1, true)")
Right (Literal (LInt 1))
λ eval <$> check (parseExpr "(\\x : (Int, Bool) . snd x) (1, true)")
Right (Literal (LBool True))
```

### 8/22/17

Today I worked on implementing sum types in the language and this required a bit more learning and experimenting on my part. Initially I had some confusion over sum and product types, but a little guidance from @robrix straightened me out. The two concepts are really mirror images of each other. The pairs I implemented yesterday use `,` to build up a new pair and use `fst` or `snd` to tear down a pair. Pairs are essentially product types. Sums on the other hand are constructed or built up with `left` and `right` and torn down with `case` (in this toy language at least). You can see that they reflect each other.

A small detour for the mathematical notion behind sums and products. In Haskell, sum types look like this (basically `Either`):

``` haskell
data Sum a b = L a | R b
```

which means the space of available types increases arithmetically:

``` haskell
data Zero
data One = One
type Two = Sum One One
type Three = Sum One (Sum One One)
```

Product types on the other hand look like this:

``` haskell
data Product a b = P a b
```

which means the space of available types multiplies:

``` haskell
type TwoByTwo = Product Two Two
-- Possible types look like this:
-- P (L One) (L One)
-- P (L One) (R One)
-- P (R One) (R One)
-- P (R One) (L One)
```

You can of course define Zero:

``` haskell
data Zero
```

And wonderfully, functions are exponential types:

``` haskell
type Exponential a b = a -> b
```

Moving on.....

So for this toy language I set out to create a few new constructs to model sums, namely:

- `left` and `right` constructors
- `case .. of` statements to pattern match on the two branches.

The tricky part for me was modeling the various places where the types need to line up in the type checker. I ended up with `case` holding an expression and two lambdas (left and right) and that the constructors `left`/`right` would hold a lambda and their return type. Here's what that looks like with the rest of the language expressions omitted for clarity:

``` haskell
data ExprF a
  = InL a Type
  | InR a Type
  | Case { expression :: a, left :: a, right :: a}
  ...

data Type
  = TInt
  | TBool
  | TSum Type Type
  ...
```

(My first attempt put the `Type` on the `Case` as instead, but then I didn't have enough information when type checking.)

When it comes to checking the types for a `case` statement, there are a bunch of things you have to validate:

- The case expression has to return a sum type.
- Both the left and right branches of the case have to return the same type.
- The left branch has to accept an argument of the same type as the left of the case expression's sum type.
- The right branch has to accept an argument of the same type as the right of the case expression's sum type.

My language also requires that both left and right branches are lambdas and that `left` and `right` be constructed with the full sum type annotation. For example, even if you want to have just `left 1`, you have to fully specify the overall sum type b/c that type is then used to check consistency. So instead, you need to write `left 1 : (Int + Bool)` where `Bool` is the type of the right part of the sum (could be anything as long as it's consistent).

Once done with the type checking, evaluation looks a lot like function application, but you need some new values to keep track of whether the left or the right branch needs to be evaluated. I got a little turned around thinking through the evaluation here, but it helps to write how you'd do it in Haskell with Either and functions.

``` haskell
case' :: Either l r -> (l -> a) -> (r -> a) -> a
case' e fL fR = case e of
  Left l -> fL l
  Right r -> fR r
```

The biggest difference in writing the evaluation for `case` is that you have to eval the left or right branch and then apply the arg by evaluating the closure body in the new environment. That looks a bit like this (rest of `eval` omitted, see `src/Interpreter.hs`)

``` haskell
...
InL a _ -> VLeft (eval' env a)
InR a _ -> VRight (eval' env a)
Case e ifL ifR -> case eval' env e of
  VLeft arg  -> let Closure n body env' = eval' env ifL
                in eval' ((n, arg) : env') body
  VRight arg -> let Closure n body env' = eval' env ifR
                in eval' ((n, arg) : env') body
...
```

And that's it. Here's some sum types in action:

```sh
λ eval <$> check (parseExpr "(\\x : (Int + Bool) . case x of (\\x : Int . true) (\\x : Bool . false)) (left 1 : (Int + Bool))")
Right (Literal (LBool True))
λ eval <$> check (parseExpr "(\\x : (Int + Bool) . case x of (\\x : Int . true) (\\x : Bool . false)) (right true : (Int + Bool))")
Right (Literal (LBool False))

# We expect this to fail type checking.
λ eval <$> check (parseExpr "(\\x : (Int + Bool) . case x of (\\x : Int . true) (\\x : Bool . false)) 1")
Left "expected type TSum TInt TBool but got TInt"

# We expect this to fail type checking because x isn't a sum type.
λ eval <$> check (parseExpr "(\\x : Int . case x of (\\x : Int . true) (\\x : Bool . false)) 1")
Left "don't know how to handle case with non-sum types"

# We expect this to fail type checking because the left branch takes a pair (Int, Int), but the case is for the sum type (Int + Bool).
λ eval <$> check (parseExpr "(\\x : (Int + Bool) . case x of (\\x : (Int, Int) . true) (\\x : Bool . false)) (right true : (Int + Bool))")
Left "nope TSum TInt TBool. left is TPair TInt TInt -> TBool. right is TBool -> TBool."
```

Tada!

### 8/28/2017

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





## References:

- A lot of the ideas and code here is inspired by and directly from [Stephen Diehl](http://dev.stephendiehl.com/fun).
- [Church booleans](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans)
- [Fixed point combinators](https://en.wikipedia.org/wiki/Fixed-point_combinator)
