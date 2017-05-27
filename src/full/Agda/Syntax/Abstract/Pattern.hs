{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Agda.Syntax.Abstract.Pattern where

import Data.Foldable
import Data.Functor
import Data.Monoid

import Agda.Syntax.Common
import Agda.Syntax.Concrete (FieldAssignment', exprFieldA)
import Agda.Syntax.Abstract as A

type NAP = NamedArg Pattern

class MapNamedArgPattern a  where
  mapNamedArgPattern :: (NAP -> NAP) -> a -> a

  default mapNamedArgPattern
     :: (Functor f, MapNamedArgPattern a', a ~ f a') => (NAP -> NAP) -> a -> a
  mapNamedArgPattern = fmap . mapNamedArgPattern

instance {-# OVERLAPPING #-} MapNamedArgPattern NAP where
  mapNamedArgPattern f p =
    case namedArg p of
      -- no sub patterns:
      VarP{}    -> f p
      WildP{}   -> f p
      DotP{}    -> f p
      LitP{}    -> f p
      AbsurdP{} -> f p
      ProjP{}   -> f p
      -- list of NamedArg subpatterns:
      ConP i qs ps       -> f $ setNamedArg p $ ConP i qs $ mapNamedArgPattern f ps
      DefP i qs ps       -> f $ setNamedArg p $ DefP i qs $ mapNamedArgPattern f ps
      PatternSynP i x ps -> f $ setNamedArg p $ PatternSynP i x $ mapNamedArgPattern f ps
      -- Pattern subpattern(s):
      -- RecP: we copy the NamedArg info to the subpatterns but discard it after recursion
      RecP i fs          -> f $ setNamedArg p $ RecP i $ map (fmap namedArg) $ mapNamedArgPattern f $ map (fmap (setNamedArg p)) fs
      -- AsP: we hand the NamedArg info to the subpattern
      AsP i x p0         -> f $ updateNamedArg (AsP i x) $ mapNamedArgPattern f $ setNamedArg p p0

instance MapNamedArgPattern LHSCore where
  mapNamedArgPattern f lhscore =
    case lhscore of
      LHSHead q ps      -> LHSHead q $ mapNamedArgPattern f ps
      LHSProj q core ps -> LHSProj q (mapNamedArgPattern f core) $ mapNamedArgPattern f ps

instance MapNamedArgPattern a => MapNamedArgPattern (Named x a) where
instance MapNamedArgPattern a => MapNamedArgPattern (Arg a) where
instance MapNamedArgPattern a => MapNamedArgPattern [a] where
instance MapNamedArgPattern a => MapNamedArgPattern (FieldAssignment' a) where
instance MapNamedArgPattern a => MapNamedArgPattern (Maybe a) where


-- | Collect something from @NamedArg a@.

class FoldNamedArg b where
  foldNamedArg :: Monoid m => (forall a. NamedArg a -> m) -> b -> m

  default foldNamedArg
    :: (Monoid m, Foldable f, FoldNamedArg b', b ~ f b')
    => (forall a. NamedArg a -> m) -> b -> m
  foldNamedArg = foldMap . foldNamedArg

instance FoldNamedArg b => FoldNamedArg [b]                  where
instance FoldNamedArg b => FoldNamedArg (FieldAssignment' b) where
instance FoldNamedArg b => FoldNamedArg (Maybe b)            where

instance FoldNamedArg a => FoldNamedArg (NamedArg a) where
  foldNamedArg f x = f x `mappend` foldNamedArg f (namedArg x)

instance FoldNamedArg Pattern where
  foldNamedArg f p =
    case p of
      -- no sub patterns:
      VarP{}    -> mempty
      WildP{}   -> mempty
      DotP{}    -> mempty
      LitP{}    -> mempty
      AbsurdP{} -> mempty
      ProjP{}   -> mempty
      -- list of NamedArg subpatterns:
      ConP i qs ps       -> foldNamedArg f ps
      DefP i qs ps       -> foldNamedArg f ps
      PatternSynP i x ps -> foldNamedArg f ps
      -- Pattern subpattern(s):
      RecP i fs          -> foldNamedArg f fs
      AsP i x p0         -> foldNamedArg f p0

instance FoldNamedArg LHSCore where
  foldNamedArg f lhs =
    case lhs of
      LHSHead _ ps -> foldNamedArg f ps
      LHSProj _ p ps -> foldNamedArg f p `mappend` foldNamedArg f ps
