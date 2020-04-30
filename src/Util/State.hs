module Util.State where

import Data.Map
import StaticCheck.Format

getNewLoc :: S -> (Int, S)
getNewLoc (S varsMap loc funArgs atStoreMap) = (loc + 1, S varsMap (loc + 1) funArgs atStoreMap)

putInLoc :: Int -> (E, FType, FValueStatement) -> S -> S
putInLoc loc thing (S varsMap newIntLoc funArgs atStoreMap) =
  S (insert loc thing varsMap) newIntLoc funArgs atStoreMap

stateLookup :: Int -> S -> (E, FType, FValueStatement)
stateLookup loc (S varsMap _ _ _) = varsMap ! loc

funArgNamesLookup :: S -> Int -> [FPatternMatch]
funArgNamesLookup (S _ _ funArgs _) loc =
  if member loc funArgs
    then funArgs ! loc
    else []

putArgNames :: S -> Int -> [FPatternMatch] -> S
putArgNames (S varsMap newIntLoc functionArgsMap atStoreMap) loc strs = 
  S varsMap newIntLoc (insert loc strs functionArgsMap) atStoreMap

addTypeConstructor :: String -> Int -> S -> S
addTypeConstructor = undefined

getTypeConstructorArity :: String -> S -> Int
getTypeConstructorArity = undefined