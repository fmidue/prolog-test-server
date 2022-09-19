module Util (fixupShow) where

import Data.List.Extra (replace)

fixupShow :: String -> String
fixupShow =
  replace ":-" " :- "
  . filter (/= ' ')
