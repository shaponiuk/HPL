module Util.Env where

import Data.Map
import StaticCheck.Format
import Debug.Trace

-- bool - only if it is an instance of a pm function
registerLoc :: Bool -> E -> String -> Int -> E
registerLoc b (E nameMap) name loc =
  if b && member name nameMap
    then
      let
        locs = nameMap ! name
      in E (insert name (loc:locs) nameMap)
    else
      E (insert name [loc] nameMap)

lookupLoc :: String -> E -> [Int]
lookupLoc name (E nameMap) =
  if member name nameMap
    then nameMap ! name
    else trace ("lookupLoc " ++ show name) undefined

lookupFirstLoc :: String -> E -> Int
lookupFirstLoc name env = head $ lookupLoc name env