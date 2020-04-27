module StaticCheck.CheckStructs where

import StaticCheck.Format
import Data.Map as M

checkStructs :: ATProgramFormat -> Either String SATProgramFormat
checkStructs (ATSIT structList interfaces types) = do
    map <- makeMap structList
    return $ SATSIT map interfaces types

makeMap :: [FStruct] -> Either String (M.Map String FStruct)
makeMap [] = return empty
makeMap (s@(FStructB name _):xs) = do
    map <- makeMap xs
    if M.member name map 
        then fail "UUU TODO" 
        else return $ M.insert name s map
makeMap (s@(FStructI name _ _):xs) = do
    map <- makeMap xs
    if M.member name map
        then fail "UUU TODO"
        else return $ M.insert name s map