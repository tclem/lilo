{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Pretty where

import Syntax
import Text.PrettyPrint

class Pretty p where
  ppr :: Int -> p -> Doc

  -- pp :: p -> Doc
  -- pp = ppr 0

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

instance Pretty Name where
  ppr _ = text

instance Pretty Expr where
  ppr p e = case out e of
    Lit (LInt a) -> text (show a)
    Lit (LBool b) -> text (show b)
    Var x -> text x
    App a b -> parensIf (p > 0) $ ppr (p + 1) a <+> ppr p b
    Lam n ty body -> parensIf (p > 0) $
          char '\\'
      <>  text n <> text ":" <> ppr (p + 1) ty
      <+> text "->"
      <+> ppr (p + 1) body

instance Pretty Type where
  ppr p ty = case ty of
    TVar x -> text x
    TInt -> text "Int"
    TBool -> text "Bool"
    TArr a b -> parens (ppr p a <+> text "->" <+> ppr p b)

pp :: Expr -> String
pp = render . ppr 0
