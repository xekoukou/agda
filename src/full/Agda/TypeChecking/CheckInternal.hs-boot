module Agda.TypeChecking.CheckInternal where

import Agda.Syntax.Common
import Agda.Syntax.Internal
import Agda.TypeChecking.Monad.Base (TCM)

data Action = Action
  { preAction  :: Type -> Term -> TCM Term
  , postAction :: Type -> Term -> TCM Term
  , relevanceAction :: Relevance -> Relevance -> Relevance
  }
fixUnusedArgAction :: Action
defaultAction :: Action

checkType :: Type -> TCM ()
checkInternal :: Term -> Type -> TCM ()
checkInternal' :: Action -> Term -> Type -> TCM Term
infer :: Term -> TCM Type
