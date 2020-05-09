module Run.OneStepEvaluation where

import StaticCheck.Format
import Util.State
import Util.Env
import Util.Util

oneStepEvaluation :: S -> E -> FValueStatement -> IO (S, E, FValueStatement)
oneStepEvaluation s e (FAValueStatement (FFunApplicationR locs argVss)) = oneStepTryRunVSFunApplR locs argVss s
oneStepEvaluation s e vs = traceD (show vs) undefined

oneStepTryRunVSFunApplR :: [Int] -> [FValueStatement] -> S -> IO (S, E, FValueStatement)
oneStepTryRunVSFunApplR [] _ _ = undefined
-- todo: hard to find the function name, probably unneeded, only one at the time
oneStepTryRunVSFunApplR (x:xs) args state = do
    let (ifFunction, env, _, vs) = stateLookup x state
    let argNames = funArgNamesLookup state x
    fits <- fitPatternMatchs state env argNames args
    if fits
        then
            if length args > length argNames
                then do
                    (e, updatedVS, b, pms) <- appendFAVS [x] args state
                    if b
                        then
                            getOneStepRegisterTriple e state pms args vs
                        else
                            return (state, e, updatedVS)
                else
                    getOneStepRegisterTriple env state argNames args vs
        else 
            oneStepTryRunVSFunApplR xs args state

getOneStepRegisterTriple :: E -> S -> [FPatternMatch] -> [FValueStatement] -> FValueStatement -> IO (S, E, FValueStatement)
getOneStepRegisterTriple e state pms args vs = do
    (newEnv, newState) <- registerArgs e state pms args
    return (newState, newEnv, vs)

registerArgs :: E -> S -> [FPatternMatch] -> [FValueStatement] -> IO (E, S)
registerArgs env state argNames vss = 
    foldl registerArgsInFoldF (return (env, state)) $ dList argNames vss

registerArgsInFoldF :: IO (E, S) -> (FPatternMatch, FValueStatement) -> IO (E, S)
registerArgsInFoldF acc (FPatternMatchB str, vs) = do
    (e, s) <- acc
    let (newLoc, newState) = getNewLoc s
    let newEnv = registerLoc False e str newLoc
    let newerState = putInLoc newLoc (False, newEnv, FTypeT [], vs) newState
    return (newEnv, newerState)
registerArgsInFoldF acc (FPatternMatchI _, _) = acc
registerArgsInFoldF acc (FPatternMatchC _ pms, FCValueStatement _ vss) = do
    (e, s) <- acc
    registerArgs e s pms vss
registerArgsInFoldF acc (FPatternMatchC cname pms, FAValueStatement (FFunApplicationR loc argVss)) = do
    (e, s) <- acc
    (ns, ne, nvs) <- oneStepEvaluation s e $ FAValueStatement (FFunApplicationR loc argVss)
    registerArgsInFoldF (return (ne, ns)) (FPatternMatchC cname pms, nvs)
registerArgsInFoldF _ el = traceD ("registerArgsInFoldF " ++ show el) undefined

appendFAVS :: [Int] -> [FValueStatement] -> S -> IO (E, FValueStatement, Bool, [FPatternMatch])
appendFAVS [] vss state = fail "Non exhaustive pattern matches"
appendFAVS (loc:xs) addVss state = do
    let argNames = funArgNamesLookup state loc
    let (f, e, _, vs) = stateLookup loc state
    fits <- fitPatternMatchs state e argNames addVss
    if fits
        then
            let
                (appendedVS, b, pms) = appendFAVSInt vs addVss
            in return (e, appendedVS, b, pms)
        else 
            appendFAVS xs addVss state

appendFAVSInt :: FValueStatement -> [FValueStatement] -> (FValueStatement, Bool, [FPatternMatch])
appendFAVSInt (FAValueStatement (FFunApplicationB funName vss)) addVss = (FAValueStatement $ FFunApplicationB funName (vss ++ addVss), False, [])
appendFAVSInt (FFValueStatement name vs) addVSS = (vs, True, [FPatternMatchB name])
appendFAVSInt x y = traceD ("appendFAVSInt " ++ show x ++ "\n" ++ show y) undefined

fitPatternMatchs :: S -> E -> [FPatternMatch] -> [FValueStatement] -> IO Bool
fitPatternMatchs s e pms vss = ioAll (fitPatternMatch s e) $ dList pms vss

fitPatternMatch :: S -> E -> (FPatternMatch, FValueStatement) -> IO Bool
fitPatternMatch _ _ (FPatternMatchI i1, FIValueStatement i2) = return $ i1 == i2
fitPatternMatch s e a@(FPatternMatchC (FPatternMatchB name1) pms, FCValueStatement name2 vss) = do
    res1 <- fitPatternMatchs s e pms vss
    return $ name1 == name2 && res1
fitPatternMatch _ _ (FPatternMatchB _, _) = return True
fitPatternMatch s e (FPatternMatchC (FPatternMatchB name1) pms, FAValueStatement (FFunApplicationR loc argVss)) = do
    (ns, ne, nvs) <- oneStepEvaluation s e $ FAValueStatement (FFunApplicationR loc argVss)
    fitPatternMatch ns ne (FPatternMatchC (FPatternMatchB name1) pms, nvs)
fitPatternMatch _ _ a = traceD ("fitPatternMatch undefined " ++ show a) undefined