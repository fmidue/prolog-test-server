module Util (fixupShow, Mode(..)) where

import Data.List.Extra (replace)

data Mode = Indiviual Int | Summary

fixupShow :: String -> String
fixupShow =
  replace ":-" " :- "
  . filter (/= ' ')
