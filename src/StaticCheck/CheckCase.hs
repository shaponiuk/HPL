module StaticCheck.CheckCase where

import StaticCheck.Format
import Bnfc.ErrM
import Data.Char

checkLowerCaseFunctionDefs :: [FFunctionDef] -> Err ()
checkLowerCaseFunctionDefs [] = return ()
checkLowerCaseFunctionDefs (x:xs) = do
    let name@(c:_) = getFunctionName x
    if isLower c
        then checkLowerCaseFunctionDefs xs
        else fail $ "function " ++ name ++ " doesn't start with a lowercase letter"

checkLowerCaseRefDefs :: [FRefDef] -> Err ()
checkLowerCaseRefDefs [] = return ()
checkLowerCaseRefDefs (FRefDef (Just pos) _ name@(c:_) _:xs) =
    if isLower c
        then checkLowerCaseRefDefs xs
        else fail $ "reference " ++ name ++ " doesn't start with a lowercase letter " ++ show pos

checkLowerArgNames :: String -> [String] -> Err ()
checkLowerArgNames _ [] = return ()
checkLowerArgNames name (arg@(c:_):args) =
    if isLower c
        then checkLowerArgNames name args
        else fail $ "type argument " ++ arg ++ " in algebraic type " ++ name ++ " doesn't start with a lowercase letter"

checkUpperAlgTypeVals :: String -> [FAlgTypeVal] -> Err ()
checkUpperAlgTypeVals _ [] = return ()
checkUpperAlgTypeVals name (FAlgTypeVal (Just pos) constructorName@(c:_) _:algTypeVals) =
    if isUpper c
        then checkUpperAlgTypeVals name algTypeVals
        else fail $ "type constructor " ++ constructorName ++ " in algebraic type " ++ name ++ " doesn't start with an uppercase letter " ++ show pos

checkUpperCaseAlgTypes :: [FAlgType] -> Err ()
checkUpperCaseAlgTypes [] = return ()
checkUpperCaseAlgTypes (FAlgType (Just pos) name@(c:_) argNames algTypeVals:xs) =
    if isUpper c
        then do
            checkLowerArgNames name argNames
            checkUpperAlgTypeVals name algTypeVals
            checkUpperCaseAlgTypes xs
        else fail $ "algebraic type " ++ name ++ " doesn't start with an uppercase letter " ++ show pos

checkCase :: ProgramFormat -> Err ProgramFormat
checkCase pf@(SITList functionDefs refDefs algTypes) = do
    checkLowerCaseFunctionDefs functionDefs
    checkLowerCaseRefDefs refDefs
    checkUpperCaseAlgTypes algTypes
    return pf