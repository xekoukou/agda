{-# LANGUAGE CPP #-}

-- | Compute eta short normal forms.
module Agda.TypeChecking.EtaContract where

import Agda.Syntax.Common
import Agda.Syntax.Internal
import Agda.Syntax.Internal.Generic
import Agda.TypeChecking.Substitute
import Agda.TypeChecking.Free
import Agda.TypeChecking.Monad
import Agda.TypeChecking.Records
import Agda.Utils.Monad

#include "../undefined.h"
import Agda.Utils.Impossible

-- TODO: move to Agda.Syntax.Internal.SomeThing
data BinAppView = App Term (Arg Term)
                | NoApp Term

binAppView :: Term -> BinAppView
binAppView t = case t of
  Var i xs   -> app (Var i) xs
  Def c xs   -> app (Def c) xs
  Con c xs   -> app (Con c) xs
  Lit _      -> noApp
  Lam _ _    -> noApp
  Pi _ _     -> noApp
  Fun _ _    -> noApp
  Sort _     -> noApp
  MetaV _ _  -> noApp
  where
    noApp = NoApp t
    app f [] = noApp
    app f xs = App (f $ init xs) (last xs)

etaContract :: (MonadTCM tcm, TermLike a) => a -> tcm a
etaContract = traverseTermM eta
  where
    eta t@(Lam h b) = case binAppView (absBody b) of
      App u (Arg _ (Var 0 []))
        | not (freeIn 0 u)  -> return $ subst __IMPOSSIBLE__ u
      _ -> return t
    eta t@(Con r args) =
      ifM (isRecord r)
          (etaContractRecord r args)
          (return t)
    eta t = return t

