### Syntax à la carte

*9/22/2017*

For my next challenge, I wanted to incorporate a more flexible approach of defining the syntax expressions of the lilo lambda calculus using the techniques described in the paper [Data types à la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf).

If you'll remember the very initial definition of `Expr` as the starting reference point:

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

To restate some of the core concepts of the *Data types à la carte* functional peal, we could extend this data type (as I did for pairs and sums), but doing so requires that we recompile the code. Instead


> The goal is to define a data type by cases, where one can add new cases to the data type and new functions over the data type, without recompiling existing code, and while retaining static type safety.
  - Phil Wadler (1998), the *Expression Problem*

Instead, we define `Expr` as follows:

``` haskell
newtype Expr f = In (f (Expr f))
```

Note that `Expr` is now parameterized by *f* which is going to be the signature of our constructors. We are now free to define the various parts of the language like so:

``` haskell
newtype Boolean a = Boolean Bool deriving (Functor)
newtype Integer a = Integer Int deriving (Functor)
newtype Variable a = Variable String deriving (Functor)
data Lambda a = Lambda String a deriving (Functor)
data Application a = Application a a deriving (Functor)
```

Now, in order to combine expressions, the paper relies on taking the co-product of their signatures. We can take that one step further and use an implementation of an Open Union which is really giving us type-indexed co-products.

Using these techniques, I then set out to implement pretty printing and evaluation.

#### Pretty printing

Pretty printing is straight forward and sets the stage for how we will work with the à la carte data types. We define a type class `Render` and then each individual syntax data type implements their own instance of render.

```
λ pp (lam "x" (var "x") :: Expr (Union '[Syntax.Integer, Boolean, Variable, Lambda, Application]))
"\\x -> x"
λ pp (app (lam "x" (var "x")) (int 1) :: Expr (Union '[Syntax.Integer, Boolean, Variable, Lambda, Application]))
"(\\x -> x) 1"
```

#### Evaluating

Evaluating turned out to be slightly tricker for me to implement, largely due to the need for a more general recursion than the simple fold and algebra presented in the paper. I also ended up wiring up the parser again (easy to do with the `Expr (Union f)` smart constructors).

```
λ eval (lam "x" (var "x") :: Expr (Union '[Syntax.Integer, Boolean, Variable, Lambda, Application]))
Right (Closure "x" (In (Variable "x")) [])
λ eval ((var "x") :: Expr (Union '[Syntax.Integer, Boolean, Variable, Lambda, Application]))
Left "free variable x"
λ eval (app (lam "x" (var "x")) (int 1) :: Expr (Union '[Syntax.Integer, Boolean, Variable, Lambda, Application]))
Right (LInt 1)
λ eval $ parseExpr "1"
Right (LInt 1)
λ eval $ parseExpr "(\\x.x) 1"
Right (LInt 1)
λ eval $ parseExpr "(\\x.(\\y.y)) 1"
Right (Closure "y" (In (Variable "y")) [("x",LInt 1)])
```
