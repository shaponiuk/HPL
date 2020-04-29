module Util.State where

import Data.Map
import StaticCheck.Format

getNewLoc :: S -> (Int, S)
getNewLoc (S vars loc funArgs) = (loc + 1, S vars (loc + 1) funArgs)

putInLoc :: Int -> (FType, FValueStatement) -> S -> S
putInLoc loc thing (S vars newInt funArgs) =
  S (insert loc thing vars) newInt funArgs

stateLookup :: Int -> S -> (FType, FValueStatement)
stateLookup loc (S varsMap _ _) = varsMap ! loc

funArgNamesLookup :: S -> Int -> [FPatternMatch]
funArgNamesLookup (S _ _ funArgs) loc =
  if member loc funArgs
    then funArgs ! loc
    else []

putArgNames :: S -> Int -> [FPatternMatch] -> S
putArgNames (S vars newInt functionArgs) loc strs = 
  S vars newInt (insert loc strs functionArgs)