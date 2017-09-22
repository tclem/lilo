### A lambda calculus is born

*08/18/2017*

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
