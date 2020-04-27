module StaticCheck.CheckAlgTypes where

import StaticCheck.Format
import Data.Map as M

checkAlgTypes :: ProgramFormat -> Either String ATProgramFormat
checkAlgTypes (SITList structs interfaces typeList) = do
    map <- makeMap typeList
    return $ ATSIT structs interfaces map

makeMap :: [FAlgType] -> Either String (M.Map String FAlgType)
makeMap [] = return empty
makeMap (at@(FAlgType name strs atVals):xs) = do
    map <- makeMap xs
    if M.member name map 
        then fail "UUU TODO" 
        else if name /= "Sem" && name /= "Int" && name /= "String" 
            then return $ M.insert name at map
            else fail "EEE TODO"