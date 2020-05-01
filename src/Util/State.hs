module Util.State where

import Data.Map
import StaticCheck.Format

getNewLoc :: S -> (Int, S)
getNewLoc (S varsMap loc funArgs) = (loc + 1, S varsMap (loc + 1) funArgs)

putInLoc :: Int -> (Bool, E, FType, FValueStatement) -> S -> S
putInLoc loc thing (S varsMap newIntLoc funArgs) =
  S (insert loc thing varsMap) newIntLoc funArgs

stateLookup :: Int -> S -> (Bool, E, FType, FValueStatement)
stateLookup loc (S varsMap _ _) = varsMap ! loc

funArgNamesLookup :: S -> Int -> [FPatternMatch]
funArgNamesLookup (S _ _ funArgs) loc =
  if member loc funArgs
    then funArgs ! loc
    else []

putArgNames :: S -> Int -> [FPatternMatch] -> S
putArgNames (S varsMap newIntLoc functionArgsMap) loc strs = 
  S varsMap newIntLoc $ insert loc strs functionArgsMap

getUnsetLoc :: S -> [Int] -> Int
getUnsetLoc _ [] = undefined
getUnsetLoc s (x:xs) = if member x (vars s) then getUnsetLoc s xs else x