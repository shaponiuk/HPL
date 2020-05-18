module StaticCheck.CheckUniqueDefinitions where

import StaticCheck.Format
import Bnfc.ErrM
import Util.Util
import Data.Map
import Data.Char
import Control.Monad.State (lift)
import Control.Monad.Trans.State as ST
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity

newtype UniqueDefS = UniqueDefS (Map String [(Bool, String, FType, [FPatternMatch])])
    deriving (Show)

-- name -> (argTypes, type constructors)
-- type constructors
data UniqueAlgTypeS = UniqueAlgTypeS (Map String ([String], [FAlgTypeVal])) (Map String FAlgTypeVal)

hasDifferentSusOrDifferentTypeOrDifferentPMs :: Bool -> FType -> [FPatternMatch] -> [(Bool, String, FType, [FPatternMatch])] -> Bool
hasDifferentSusOrDifferentTypeOrDifferentPMs sus t pms [] = False
hasDifferentSusOrDifferentTypeOrDifferentPMs sus t pms ((sus_, _, t_, pms_) : xs)
  | sus /= sus_ = True
  | t /= t_ = True
  | length pms /= length pms_ = True
  | otherwise = hasDifferentSusOrDifferentTypeOrDifferentPMs sus t pms xs


checkUniqueFunctionDefinitions :: [FFunctionDef] -> ST.StateT UniqueDefS Err UniqueDefS
checkUniqueFunctionDefinitions [] =
    ST.get
checkUniqueFunctionDefinitions (NonSusFFunctionDef (Just pos) t name pms _ : fdefs) = do
    UniqueDefS m <- ST.get
    let registerDefs = if member name m
            then m ! name
            else []
    if hasDifferentSusOrDifferentTypeOrDifferentPMs False t pms registerDefs
        then lift $ fail $ name ++ " declarations don't match " ++ show pos
        else do
            ST.put $ UniqueDefS $ insert name ((False, name, t, pms):registerDefs) m
            checkUniqueFunctionDefinitions fdefs
checkUniqueFunctionDefinitions (SusFFunctionDef (NonSusFFunctionDef (Just pos) t name pms _) : fdefs) = do
    UniqueDefS m <- ST.get
    let registerDefs = if member name m
            then m ! name
            else []
    if hasDifferentSusOrDifferentTypeOrDifferentPMs True t pms registerDefs
        then lift $ fail $ name ++ " declarations don't match " ++ show pos
        else do
            ST.put $ UniqueDefS $ insert name ((True, name, t, pms):registerDefs) m
            checkUniqueFunctionDefinitions fdefs

checkUniqueReferenceDefinitions :: [FRefDef] -> ST.StateT UniqueDefS Err UniqueDefS
checkUniqueReferenceDefinitions [] =
    ST.get
checkUniqueReferenceDefinitions (FRefDef (Just pos) t name vs:refdefs) = do
    UniqueDefS m <- ST.get
    if member name m
        then lift $ fail $ name ++ " declarations are not unique " ++ show pos
        else do
            ST.put $ UniqueDefS $ insert name [(False, name, t, [])] m
            checkUniqueReferenceDefinitions refdefs

checkTypeArgsInType :: FType -> [String] -> ST.StateT UniqueAlgTypeS Err UniqueAlgTypeS
checkTypeArgsInType (FTypeB (Just pos) name@(x:xs) []) typeArgs =
    if (isLower x && name `elem` typeArgs) || isUpper x
        then ST.get
        else lift $ fail $ "type arg: " ++ name ++ " is unregistered " ++ show pos
checkTypeArgsInType (FTypeB (Just pos) name (x:xs)) typeArgs = do
    checkTypeArgsInType x typeArgs
    checkTypeArgsInType (FTypeB (Just pos) name xs) typeArgs
checkTypeArgsInType (FTypeT _ []) _ =
    ST.get
checkTypeArgsInType (FTypeT (Just pos) (x:xs)) typeArgs = do
    checkTypeArgsInType x typeArgs
    checkTypeArgsInType (FTypeT (Just pos) xs) typeArgs

checkUniqueAlgTypeConstructors :: [FAlgTypeVal] -> [String] -> ST.StateT UniqueAlgTypeS Err UniqueAlgTypeS
checkUniqueAlgTypeConstructors [] _ =
    ST.get
checkUniqueAlgTypeConstructors (algTypeVal@(FAlgTypeVal (Just pos) constructorName t):algTypVals) typeArgs = do
    checkTypeArgsInType t typeArgs
    UniqueAlgTypeS typeMap constructorMap <- ST.get
    if member constructorName constructorMap
        then lift $ fail $ "duplicate constructor name: " ++ constructorName ++ " " ++ show pos
        else do
            ST.put $ UniqueAlgTypeS typeMap $ insert constructorName algTypeVal constructorMap
            checkUniqueAlgTypeConstructors algTypVals typeArgs

checkUniqueAlgTypeDefinitions :: [FAlgType] -> ST.StateT UniqueAlgTypeS Err UniqueAlgTypeS
checkUniqueAlgTypeDefinitions [] =
    ST.get
checkUniqueAlgTypeDefinitions (FAlgType (Just pos) typeName typeArgs typeConstructors:algTypes) = do
    UniqueAlgTypeS typeMap constructorMap <- ST.get
    if member typeName typeMap
        then lift $ fail $ typeName ++ " algebraic type is already registered " ++ show pos
        else do
            UniqueAlgTypeS newTypeMap newConstructorMap <- checkUniqueAlgTypeConstructors typeConstructors typeArgs
            ST.put $ UniqueAlgTypeS (insert typeName (typeArgs, typeConstructors) newTypeMap) newConstructorMap
            checkUniqueAlgTypeDefinitions algTypes


runCheckUniqueFunctionDefinitions :: [FFunctionDef] -> UniqueDefS -> Err UniqueDefS
runCheckUniqueFunctionDefinitions funDefs state = do
    (_, s) <- runStateT (checkUniqueFunctionDefinitions funDefs) state
    return s

runCheckUniqueReferenceDefinitions :: [FRefDef] -> UniqueDefS -> Err UniqueDefS
runCheckUniqueReferenceDefinitions refDefs state = do
    (_, s) <- runStateT (checkUniqueReferenceDefinitions refDefs) state
    return s

runCheckUniqueAlgTypesDefinitions :: [FAlgType] -> UniqueAlgTypeS -> Err UniqueAlgTypeS
runCheckUniqueAlgTypesDefinitions algTypes state = do
    (_, s) <- runStateT (checkUniqueAlgTypeDefinitions algTypes) state
    return s

checkUniqueDefinitions :: ProgramFormat -> Err ProgramFormat
checkUniqueDefinitions pf@(SITList funDefs refDefs algTypes) = do
    s1 <- runCheckUniqueFunctionDefinitions funDefs $ UniqueDefS empty
    _ <- runCheckUniqueReferenceDefinitions refDefs s1
    _ <- runCheckUniqueAlgTypesDefinitions algTypes $ UniqueAlgTypeS empty empty
    return pf