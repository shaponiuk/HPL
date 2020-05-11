module Run.WrapFunction where

import StaticCheck.Format
import Util.State
import Util.Env
import Run.OneStepEvaluation

wrapFunction :: FValueStatement -> [FPatternMatch] -> [FValueStatement] -> E -> S -> (S, FValueStatement)
wrapFunction vs pms vss env s = undefined

wrapFunctionB :: FValueStatement -> E -> S -> IO (S, FValueStatement)
wrapFunctionB vs@(FAValueStatement (FFunApplicationB x vss)) env state = do
    let locs = lookupLoc x env
    wrapFunctionBInt locs vs env state

wrapFunctionBInt :: [Int] -> FValueStatement -> E -> S -> IO (S, FValueStatement)
wrapFunctionBInt (loc:locs) vs@(FAValueStatement (FFunApplicationB x vss)) env state = do
    let funArgNames = funArgNamesLookup state loc
    fits <- fitPatternMatchs state env funArgNames vss
    if fits
        then do
            let d = length funArgNames - length vss
            let (newLocs, newState) = getNNewLocs state d
            let lambdaNames = map show newLocs
            return (newState, wrapFunctionBIntNNewLambdas d vs lambdaNames)
        else 
            wrapFunctionBInt locs vs env state

wrapFunctionBIntNNewLambdas :: Int -> FValueStatement -> [String] -> FValueStatement
wrapFunctionBIntNNewLambdas d (FAValueStatement (FFunApplicationB x vss)) strs = 
    let
        argVss = map makeFunApplicationNoArg strs
    in foldl (flip FFValueStatement) (FAValueStatement (FFunApplicationB x (vss ++ argVss))) strs


makeFunApplicationNoArg :: String -> FValueStatement
makeFunApplicationNoArg x = FAValueStatement (FFunApplicationB x [])