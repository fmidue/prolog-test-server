{-# LANGUAGE OverloadedStrings #-}
module DropClauseMutation (dropClauseMutation, Mode) where

import Web.Scotty (Parsable(..))

import Language.Prolog

data Mode = Indiviual


instance Parsable Mode where
  parseParam "indiv" = Right Indiviual
  parseParam _ = Left "unable to parse mode"

type Error = String
type ProgramText = String

dropClauseMutation :: Mode -> Int -> ProgramText -> Either Error [ProgramText]
dropClauseMutation _ n s = case consultString s of
  Left e -> Left $ show e
  Right cs -> Right [ unlines $ show <$> dropIx i cs | i <- [1..n]]

dropIx :: Int -> [a] -> [a]
dropIx n xs = take (n-1) xs ++ drop n xs
