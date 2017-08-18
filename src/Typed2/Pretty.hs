{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Typed2.Pretty where

import Typed2.Syntax
import Text.PrettyPrint

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

viewVars :: Expr -> [Name]
viewVars (Lam n _ a) = n : viewVars a
viewVars _ = []

viewBody :: Expr -> Expr
viewBody (Lam _ _ a) = viewBody a
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
    Lam _ _ a -> parensIf (p > 0) $
        char '\\'
      <> hsep (fmap pp (viewVars e))
      <+> "->"
      <+> ppr (p + 1) (viewBody e)

ppExpr :: Expr -> String
ppExpr = render . ppr 0
