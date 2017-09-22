### Implementing Pairs

*08/21/17*

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
