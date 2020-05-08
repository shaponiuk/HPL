module Run.OneStepEvaluation where

import StaticCheck.Format
import Util.State
import Util.Env
import Util.Util

oneStepEvaluation :: S -> E -> FValueStatement -> (S, E, FValueStatement)
oneStepEvaluation s e (FAValueStatement (FFunApplicationR locs argVss)) = oneStepTryRunVSFunApplR locs argVss s
oneStepEvaluation s e vs = traceD (show vs) undefined

oneStepTryRunVSFunApplR :: [Int] -> [FValueStatement] -> S -> (S, E, FValueStatement)
oneStepTryRunVSFunApplR [] _ _ = undefined
-- todo: hard to find the function name, probably unneeded, only one at the time
oneStepTryRunVSFunApplR (x:xs) args state = do
    let (ifFunction, env, _, vs) = stateLookup x state
    let argNames = funArgNamesLookup state x
    if fitPatternMatchs state env argNames args
        then
            if length args > length argNames
                then do
                    let (e, updatedVS, b, pms) = forceUnwrapMaybe $ appendFAVS [x] args state
                    if b
                        then
                            getOneStepRegisterTriple e state pms args vs
                        else
                            (state, e, updatedVS)
                else
                    getOneStepRegisterTriple env state argNames args vs
        else 
            oneStepTryRunVSFunApplR xs args state

getOneStepRegisterTriple :: E -> S -> [FPatternMatch] -> [FValueStatement] -> FValueStatement -> (S, E, FValueStatement)
getOneStepRegisterTriple e state pms args vs =
    let (newEnv, newState) = registerArgs e state pms args in
        (newState, newEnv, vs)

registerArgs :: E -> S -> [FPatternMatch] -> [FValueStatement] -> (E, S)
registerArgs env state argNames vss = 
    result where
        result = foldl registerArgsInFoldF (env, state) $ dList argNames vss

registerArgsInFoldF :: (E, S) -> (FPatternMatch, FValueStatement) -> (E, S)
registerArgsInFoldF (e, s) (FPatternMatchB str, vs) =
    let
        (newLoc, newState) = getNewLoc s
        newEnv = registerLoc False e str newLoc
        newerState = putInLoc newLoc (False, newEnv, FTypeT [], vs) newState
    in (newEnv, newerState)
registerArgsInFoldF acc (FPatternMatchI _, _) = acc
registerArgsInFoldF (e, s) (FPatternMatchC _ pms, FCValueStatement _ vss) = 
    registerArgs e s pms vss
registerArgsInFoldF (e, s) (FPatternMatchC cname pms, FAValueStatement (FFunApplicationR loc argVss)) = 
    registerArgsInFoldF (ne, ns) (FPatternMatchC cname pms, nvs) where
        (ns, ne, nvs) = oneStepEvaluation s e $ FAValueStatement (FFunApplicationR loc argVss)
registerArgsInFoldF _ el = traceD ("registerArgsInFoldF " ++ show el) undefined

appendFAVS :: [Int] -> [FValueStatement] -> S -> Maybe (E, FValueStatement, Bool, [FPatternMatch])
appendFAVS [] vss state = Nothing
appendFAVS (loc:xs) addVss state =
    let
        argNames = funArgNamesLookup state loc
        (f, e, _, vs) = stateLookup loc state
    in if fitPatternMatchs state e argNames addVss
        then
            let
                (appendedVS, b, pms) = appendFAVSInt vs addVss
            in Just (e, appendedVS, b, pms)
        else appendFAVS xs addVss state

appendFAVSInt :: FValueStatement -> [FValueStatement] -> (FValueStatement, Bool, [FPatternMatch])
appendFAVSInt (FAValueStatement (FFunApplicationB funName vss)) addVss = (FAValueStatement $ FFunApplicationB funName (vss ++ addVss), False, [])
appendFAVSInt (FFValueStatement name vs) addVSS = (vs, True, [FPatternMatchB name])
appendFAVSInt x y = traceD ("appendFAVSInt " ++ show x ++ "\n" ++ show y) undefined

fitPatternMatchs :: S -> E -> [FPatternMatch] -> [FValueStatement] -> Bool
fitPatternMatchs s e pms vss = all (fitPatternMatch s e) $ dList pms vss

fitPatternMatch :: S -> E -> (FPatternMatch, FValueStatement) -> Bool
fitPatternMatch _ _ (FPatternMatchI i1, FIValueStatement i2) = i1 == i2
fitPatternMatch s e a@(FPatternMatchC (FPatternMatchB name1) pms, FCValueStatement name2 vss) =
    name1 == name2 && fitPatternMatchs s e pms vss
fitPatternMatch _ _ (FPatternMatchB _, _) = True
fitPatternMatch s e (FPatternMatchC (FPatternMatchB name1) pms, FAValueStatement (FFunApplicationR loc argVss)) =
    fitPatternMatch ns ne (FPatternMatchC (FPatternMatchB name1) pms, nvs) where
        (ns, ne, nvs) = oneStepEvaluation s e $ FAValueStatement (FFunApplicationR loc argVss)
fitPatternMatch _ _ a = traceD ("fitPatternMatch undefined " ++ show a) undefined