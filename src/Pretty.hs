{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Pretty where

import Syntax
import Text.PrettyPrint

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

viewVars :: Expr -> [Name]
viewVars (Lam n a) = n : viewVars a
viewVars _ = []

viewBody :: Expr -> Expr
viewBody (Lam _ a) = viewBody a
viewBody x = x

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

instance Pretty Name where
  ppr _ = text

instance Pretty Expr where
  ppr p e = case e of
    Lit (LInt a) -> text (show a)
    Lit (LBool b) -> text (show b)
    Var x -> text x
    App a b -> parensIf (p > 0) $ ppr (p + 1) a <+> ppr p b
    Lam x a -> parensIf (p > 0) $
        char '\\'
      <> hsep (fmap pp (viewVars e))
      <+> "->"
      <+> ppr (p + 1) (viewBody e)

ppExpr :: Expr -> String
ppExpr = render . ppr 0
