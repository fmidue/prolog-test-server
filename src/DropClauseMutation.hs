{-# LANGUAGE OverloadedStrings #-}
module DropClauseMutation (dropClauseMutation, Mode(..)) where

import Web.Scotty (Parsable(..))

import Language.Prolog

import Data.List.Extra

data Mode = Indiviual Int | Summary

type Error = String
type ProgramText = String

dropClauseMutation :: Mode -> ProgramText -> Either Error [ProgramText]
dropClauseMutation Summary s =
  case consultString s of
    Left e -> Left $ show e
    Right cs -> Right [ unlines . map (replace ":-" " :- " . filter (/= ' ') . show) $ dropIx i cs | i <- [1..length cs]]
dropClauseMutation (Indiviual n) s = take n <$> dropClauseMutation Summary s

dropIx :: Int -> [a] -> [a]
dropIx n xs = take (n-1) xs ++ drop n xs
