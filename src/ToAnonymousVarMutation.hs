{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module ToAnonymousVarMutation (toAnonVarMutation) where

import Language.Prolog

import Control.Applicative
import Data.List
import Util (fixupShow, Mode(..))
import Data.Maybe

type Error = String
type ProgramText = String

toAnonVarMutation :: Mode -> ProgramText -> Either Error [ProgramText]
toAnonVarMutation Summary s =
  case consultString s of
    Left e -> Left $ show e
    Right cs -> Right $ take 1000 $ map (unlines . map (fixupShow . show)) (tail $ mapM mutate cs)
      where
        mutate (Clause lhs rhs) = do
          anonVars <- anySubsetOf (nub (theVarsIn lhs ++ concatMap theVarsIn rhs))
          let subst = map (,(Var $ Wildcard $ Nothing)) anonVars
          return (Clause (apply subst lhs) (map (apply subst) rhs))

theVarsIn :: Term -> [ VariableName ]
theVarsIn (Var v@(VariableName _ _)) = [ v ]
theVarsIn (Struct _ ts) = concatMap theVarsIn ts
theVarsIn _  = []

anySubsetOf :: [a] -> [[a]]
anySubsetOf = map catMaybes . mapM (\v -> return Nothing <|> return (Just v))
