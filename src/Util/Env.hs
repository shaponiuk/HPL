module Util.Env where

import Data.Map
import StaticCheck.Format

registerLoc :: E -> String -> Int -> E
registerLoc (E nameMap) name loc =
  if member name nameMap
    then
      let
        locs = nameMap ! name
      in E (insert name (loc:locs) nameMap)
    else 
      E (insert name [loc] nameMap)

lookupLoc :: String -> E -> [Int]
lookupLoc name (E nameMap) = nameMap ! name

lookupFirstLoc :: String -> E -> Int
lookupFirstLoc name env = head $ lookupLoc name env