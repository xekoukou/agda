{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Agda.Compiler.Dedukti.Syntax where

import Agda.Syntax.Common

-- Haskell module generated mostly by the BNF converter

newtype Id = Id String
  deriving (Eq, Ord, Show, Read, Underscore)

data Program = Program Prelude [Line]
  deriving (Eq, Ord, Show, Read)

data Prelude = Prelude Id
  deriving (Eq, Ord, Show, Read)

data Line
    = Decl Head Term
    | Def Head Term
    | DefWithType Head Term Term
    | Rules [Rule]
    | Command Command
  deriving (Eq, Ord, Show, Read)

data DefOpt = DefYes | DefNo
  deriving (Eq, Ord, Show, Read)

data Head = Head DefOpt Id [Param] | OpaqueHead DefOpt Id [Param]
  deriving (Eq, Ord, Show, Read)

data Param = Param Id Term
  deriving (Eq, Ord, Show, Read)

data VarDecl = VarUntyped Id | VarTyped Id Term
  deriving (Eq, Ord, Show, Read)

data Rule = Rule [VarDecl] Term Term
  deriving (Eq, Ord, Show, Read)

data Term
    = Atom Id
    | Type
    | App Term Term
    | Pi Id Term Term
    | Fun Term Term
    | LamTyped Id Term Term
    | LamUntyped Id Term
  deriving (Eq, Ord, Show, Read)

data Command
    = CmdWhnf Term
    | CmdHnf Term
    | CmdSnf Term
    | CmdStep Term
    | CmdConv Term Term
    | CmdCheck Term Term
    | CmdInfer Term
  deriving (Eq, Ord, Show, Read)

instance Underscore Term where
  underscore = Atom underscore
  isUnderscore (Atom x) = isUnderscore x
  isUnderscore _ = False
