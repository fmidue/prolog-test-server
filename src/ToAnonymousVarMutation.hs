{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module ToAnonymousVarMutation (toAnonVarMutation) where

import Language.Prolog

import DropClauseMutation (Mode(..))

import Control.Applicative
import Data.List
import Data.Maybe

type Error = String
type ProgramText = String

toAnonVarMutation :: Mode -> ProgramText -> Either Error [ProgramText]
toAnonVarMutation Summary s =
  case consultString s of
    Left e -> Left $ show e
    Right cs -> Right $ map (concatMap ((++ ".\n") . init . tail . show)) (tail $ mapM mutate cs)
      where
        mutate (Clause lhs rhs) = do
          anonVars <- anySubsetOf (nub (theVarsIn lhs))
          let subst = map (,(Var $ Wildcard $ Nothing)) anonVars
          return (Clause (apply subst lhs) (map (apply subst) rhs))

theVarsIn :: Term -> [ VariableName ]
theVarsIn (Var v@(VariableName _ _)) = [ v ]
theVarsIn (Struct _ ts) = concatMap theVarsIn ts
theVarsIn _  = []

anySubsetOf :: [a] -> [[a]]
anySubsetOf = map catMaybes . mapM (\v -> return Nothing <|> return (Just v))
