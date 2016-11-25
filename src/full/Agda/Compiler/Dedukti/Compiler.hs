{-# LANGUAGE CPP #-}
{-# LANGUAGE OverlappingInstances #-}

module Agda.Compiler.Dedukti.Compiler where

import Control.Arrow ((***))
import Control.Monad
import Control.Monad.IO.Class

import Data.Function
import qualified Data.List as List

import Agda.Syntax.Common
import Agda.Syntax.Literal
import Agda.Syntax.Position
import Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Abstract.Pretty as A
import Agda.Syntax.Internal as I
import Agda.Syntax.Internal.Generic

import Agda.TypeChecking.Monad as I
import Agda.TypeChecking.Monad.Builtin
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Substitute
import Agda.Syntax.Translation.InternalToAbstract

import Agda.Compiler.Common
import Agda.Compiler.Dedukti.Syntax as D
import Agda.Compiler.Dedukti.Print (printTree)

import Agda.Utils.HashMap (HashMap)
import qualified Agda.Utils.HashMap as HashMap
import Agda.Utils.Functor
import Agda.Utils.List
import Agda.Utils.Monad
import Agda.Utils.Pretty (render, prettyShow)
import Agda.Utils.Singleton

#include "undefined.h"
import Agda.Utils.Impossible

compilerMain :: Interface -> TCM ()
compilerMain i = withScope_ (iInsideScope i) $ withShowAllArguments $ disableDisplayForms $ do
  -- withCurrentModule (iModuleName i) $ do
  d <- toDedukti i
  liftIO $ mapM_ putStrLn $ printDedukti d

-- Compiling internal syntax to Dedukti
------------------------------------------------------------------------

class ToDedukti i d | i -> d where
  toDedukti :: i -> TCM d

instance ToDedukti Interface D.Program where
  toDedukti i =
    D.Program <$> (D.Prelude <$> abstractToDedukti (iModuleName i))
              <*> toDedukti (iSignature i)

instance ToDedukti Signature [D.Line] where
  toDedukti (Sig secs defs rews) = do

    -- Translate definitions
    let sds = List.sortBy (compare `on` (rStart . nameBindingSite . qnameName . fst))
                (HashMap.toList defs)
    dss1 <- forM sds $ \ d@(q,_) -> do
      let r = rStart . nameBindingSite . qnameName $ q
      (r,) <$> toDedukti d

    -- Translate rewrite rules
    dss2 <- forM (concat $ HashMap.elems rews) $ \ rew -> do
      let r = rStart . getRange . rewName $ rew
      toDedukti rew <&> \ d -> (r, [d])

    let dss = map snd $ List.sortBy (compare `on` fst) $ dss1 ++ dss2
    return $ concat dss

    -- -- Translate definitions
    -- ds1 <- mapM toDedukti
    --   $ List.sortBy (compare `on` (rStart . nameBindingSite . qnameName . fst))
    --   $ HashMap.toList defs
    -- -- Translate rewrite rules
    -- ds2 <- mapM toDedukti
    --   $ List.sortBy (compare `on` (rStart . getRange . rewName))
    --   $ concat $ HashMap.elems rews

    -- return $ concat $ ds1 ++ ds2

instance ToDedukti (QName,Definition) [D.Line] where
  toDedukti (q,def) = do
    reportSDoc "dedukti" 10 $ text "type of " <+> prettyTCM q
    x <- abstractToDedukti q
    (:) <$> (D.Decl (D.Head defOpt x []) <$> toDedukti (defType def))
        <*> toDedukti (q,defn)
    where
    defn = theDef def
    defOpt = case defn of
      Function{} -> DefYes
      I.Primitive{} -> DefYes
      I.Axiom{} -> DefNo
      Datatype{} -> DefNo
      Record{} -> DefNo
      Constructor{} -> DefNo
      AbstractDefn{} -> __IMPOSSIBLE__

instance ToDedukti (QName,Defn) [D.Line] where
  toDedukti (q,defn) = do
    reportSDoc "dedukti" 10 $ text "def. of " <+> prettyTCM q
    case defn of
      I.Axiom{} -> return []
      I.Primitive{ primClauses = cs } -> concat <$> mapM (toDedukti . (q,)) cs
      Datatype{ dataClause = Just cl } -> toDedukti (q,cl)
      Datatype{ dataClause = Nothing, dataCons = cs } -> return []
      Constructor{} -> return []
      Record{ recClause = Just cl } -> toDedukti (q,cl)
      Record{ recClause = Nothing } -> return []
      Function{ funClauses = cs } -> concat <$> mapM (toDedukti . (q,)) cs
      AbstractDefn{} -> __IMPOSSIBLE__

instance ToDedukti (QName, I.Clause) [D.Line] where
  toDedukti (q, I.Clause _ tel ps Nothing _ _) = return []
  toDedukti (q, cl@(I.Clause _ tel ps (Just v) _ _)) = do
    v' <- traverseTermM fullConstructorForm v
    let cl' = cl { clauseBody = Just v' }
    A.Clause (A.LHS _ core []) [] (A.RHS rhs _) [] _ <- reify (NamedClause q False cl')
    u   <- freshName_ "_"
    let p = A.lhsCoreToPattern (A.Var u <$ core) -- replace dot patterns by underscore
    l   <- abstractToDedukti $ A.patternToExpr p
    r   <- abstractToDedukti rhs
    xs  <- toDedukti tel
    return [D.Rules [D.Rule xs l r]]

-- instance ToDedukti (QName, I.Clause) [D.Line] where
--   toDedukti (q, I.Clause _ tel ps Nothing _ _) = return []
--   toDedukti (q, I.Clause _ tel ps (Just v) _ _) = do
--     xs  <- toDedukti tel
--     aps <- addContext tel $ reifyPatterns ps
--     u   <- freshName_ "_"
--     let ap = A.DefP __IMPOSSIBLE__ (AmbQ [q]) aps
--         p  = A.Var u <$ ap  -- replace dot patterns by underscore
--     l   <- abstractToDedukti $ A.patternToExpr p
--     r   <- addContext tel $ toDedukti v
--     return [D.Rules [D.Rule xs l r]]

instance ToDedukti RewriteRule D.Line where
  toDedukti (RewriteRule q tel f ps r _) = notYetImplemented q

instance ToDedukti I.Telescope [D.VarDecl] where
  toDedukti tel = mapM (toDedukti . unDom) $ telToList tel  -- WRONG de Bruijn indices

instance ToDedukti (ArgName, I.Type) D.VarDecl where
  toDedukti (x, t) = return $ D.VarUntyped $ varName $ D.Id x
--  toDedukti (x, t) = D.VarTyped (D.Id x) <$> toDedukti t

instance ToDedukti I.Type D.Term where
  toDedukti = toDedukti . unEl

instance ToDedukti I.Term D.Term where
  toDedukti v = do
    reportSDoc "dedukti.term" 20 $ text "v  = " <+> prettyTCM v
    v' <- traverseTermM fullConstructorForm v
    reportSDoc "dedukti.term" 25 $ text "v' = " <+> prettyTCM v'
    abstractToDedukti =<< reify v'

-- | Expand a toplevel NATURAL literal @n@ to @suc^n zero@.
fullConstructorForm :: I.Term -> TCM I.Term
fullConstructorForm v = do
  case ignoreSharing v of
    I.Lit (LitNat r n) | n >= 0 -> do
      zero <- primZero
      suc  <- primSuc
      return $ iterate (apply1 suc) zero !! fromIntegral n
    _ -> return v

-- Printing abstract syntax to Dedukti
------------------------------------------------------------------------

class AbstractToDedukti a d where
  abstractToDedukti :: a -> TCM d

instance AbstractToDedukti A.Name D.Id where
  abstractToDedukti = (D.Id . render) <.> prettyTCM
--  abstractToDedukti = return . D.Id . prettyShow

instance AbstractToDedukti A.QName D.Id where
  abstractToDedukti = (D.Id . render) <.> prettyTCM
--  abstractToDedukti = return . D.Id . prettyShow

instance AbstractToDedukti A.ModuleName D.Id where
  abstractToDedukti = (D.Id . render) <.> prettyTCM

instance AbstractToDedukti A.AmbiguousQName D.Id where
  abstractToDedukti = (D.Id . render) <.> prettyTCM . headWithDefault __IMPOSSIBLE__ . unAmbQ
--  abstractToDedukti = return . D.Id . prettyShow . headWithDefault __IMPOSSIBLE__ . unAmbQ

-- instance AbstractToDedukti A.Pattern D.Term where
--   abstractToDedukti e =
--     A.VarP

instance AbstractToDedukti A.Expr D.Term where
  abstractToDedukti e =
    case e of
      A.Var x -> D.Atom . varName <$> abstractToDedukti x
      A.Def x -> D.Atom <$> abstractToDedukti x
      A.Proj _ x -> D.Atom . prefixId "p_" <$> abstractToDedukti x
      A.Con x -> D.Atom <$> abstractToDedukti x
      A.PatternSyn x -> D.Atom <$> abstractToDedukti x
      A.Macro x -> D.Atom <$> abstractToDedukti x
      A.Lit{} -> unsupported e
      A.QuestionMark{} -> return underscore
      A.Underscore{} -> return underscore
      A.Dot _ e -> abstractToDedukti e
      A.App _ e nes -> D.App <$> abstractToDedukti e <*> abstractToDedukti nes
      A.WithApp{} -> __IMPOSSIBLE__
      A.Lam _ xs e -> List.foldr dLam <$> abstractToDedukti e <*> abstractToDedukti xs
      A.AbsurdLam{} -> __IMPOSSIBLE__ -- notYetImplemented e
      A.ExtendedLam{} -> __IMPOSSIBLE__
      A.Fun _ a b -> D.Fun <$> abstractToDedukti a <*> abstractToDedukti b
      A.Pi _ tel b -> List.foldr (uncurry D.Pi) <$> abstractToDedukti b <*> abstractToDedukti tel
      A.Prop{}   -> __IMPOSSIBLE__
      A.Set _ _ -> return D.Type
      A.ETel{} -> __IMPOSSIBLE__
      A.Rec{} -> __IMPOSSIBLE__
      A.RecUpdate{} -> __IMPOSSIBLE__
      A.Let{} -> __IMPOSSIBLE__
      A.ScopedExpr scope e -> withScope_ scope $ abstractToDedukti e
      A.QuoteGoal{} -> __IMPOSSIBLE__
      A.QuoteContext{} -> __IMPOSSIBLE__
      A.Quote{} -> __IMPOSSIBLE__
      A.QuoteTerm{} -> __IMPOSSIBLE__
      A.Unquote{} -> __IMPOSSIBLE__
      A.Tactic{} -> __IMPOSSIBLE__
      A.DontCare e -> abstractToDedukti e

    where
    dLam (x, Nothing) = D.LamUntyped x
    dLam (x, Just a)  = D.LamTyped x a

    prefixId p (Id s) = Id $ p ++ s

varName :: D.Id -> D.Id
varName x@(Id "_") = x
varName (Id s) = Id $ "_" ++ s

instance AbstractToDedukti A.LamBinding [(D.Id, Maybe D.Term)] where
  abstractToDedukti b =
    case b of
      A.DomainFree _ x -> abstractToDedukti x <&> \ y -> [(varName y,Nothing)]
      A.DomainFull xs  -> map (varName *** Just) <$> abstractToDedukti xs

instance AbstractToDedukti A.TypedBinding [(D.Id, D.Term)] where
  abstractToDedukti b =
    case b of
      A.TBind _ xs e -> do
        d <- abstractToDedukti e
        map ((,d) . varName) <$> abstractToDedukti xs
      A.TLet{} -> unsupported b

instance AbstractToDedukti A.TypedBindings [(D.Id, D.Term)] where
  abstractToDedukti (A.TypedBindings _ a) = abstractToDedukti a

instance -- {-# OVERLAPPING #-}
  AbstractToDedukti A.Telescope [(D.Id, D.Term)] where
  abstractToDedukti tel = concat <$> mapM abstractToDedukti tel

instance AbstractToDedukti a d => AbstractToDedukti (Arg a) d where
  abstractToDedukti = abstractToDedukti . unArg

instance AbstractToDedukti a d => AbstractToDedukti (Dom a) d where
  abstractToDedukti = abstractToDedukti . unDom

instance AbstractToDedukti a d => AbstractToDedukti (Named x a) d where
  abstractToDedukti = abstractToDedukti . namedThing

instance AbstractToDedukti a d => AbstractToDedukti (WithHiding a) d where
  abstractToDedukti (WithHiding _ a) = abstractToDedukti a

instance -- {-# OVERLAPPABLE #-}
  AbstractToDedukti a d => AbstractToDedukti [a] [d] where
  abstractToDedukti = mapM abstractToDedukti

unsupported :: (HasRange a, PrettyTCM a) => a -> TCM c
unsupported a = traceCall (SetRange $ getRange a) $ do
  typeError . GenericDocError =<< do
    text "The dedukti backend does not support " <+> prettyTCM a

notYetImplemented :: (HasRange a, PrettyTCM a) => a -> TCM c
notYetImplemented a = traceCall (SetRange $ getRange a) $ do
  typeError . GenericDocError =<< do
    text "The dedukti backend does not yet support " <+> prettyTCM a


-- Line-based printing of dedukti module

class PrintDedukti d where
  printDedukti :: d -> [String]

instance PrintDedukti D.Program where
  printDedukti (D.Program h ds) =
    "(;;;;;;;;;;;;;;;;;;;;;;;;; Generated by Agda ;;;;;;;;;;;;;;;;;;;;;;;;;)" : "" :
    printDedukti h ++ [""] ++ printDedukti ds ++ [ "", "(; EOF ;)"]

instance PrintDedukti D.Prelude where
  printDedukti = singleton . printTree

instance PrintDedukti D.Line where
  printDedukti = singleton . printTree

instance PrintDedukti [D.Line] where
  printDedukti [] = []
  printDedukti [d] = singleton . printTree $ d
  printDedukti (d : d' : ds) = [ printTree d ] ++ sep ++ printDedukti (d' : ds)
    where sep = if newLineBetween d d' then [""] else []

newLineBetween :: D.Line -> D.Line -> Bool
newLineBetween d d' =
  case (d,d') of
    (D.Decl (D.Head df _ _) _, D.Decl (D.Head df' _ _) _) -> df /= df'
    (D.Decl{}, D.Def{}) -> True
    (D.Decl{}, D.DefWithType{}) -> True
    (D.Decl{}, D.Command{}) -> True
    (D.Def{}, D.Decl{}) -> True
    (D.Def{}, D.Rules{}) -> True
    (D.Def{}, D.Command{}) -> True
    (D.DefWithType{}, D.Decl{}) -> True
    (D.DefWithType{}, D.Rules{}) -> True
    (D.DefWithType{}, D.Command{}) -> True
    (D.Rules{}, D.Decl{}) -> True
    (D.Rules{}, D.Def{}) -> True
    (D.Rules{}, D.DefWithType{}) -> True
    (D.Rules{}, D.Command{}) -> True
    (D.Command{}, D.Decl{}) -> True
    (D.Command{}, D.Def{}) -> True
    (D.Command{}, D.DefWithType{}) -> True
    (D.Command{}, D.Rules{}) -> True
    _ -> False
