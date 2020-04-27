module StaticCheck.CheckInterfaces where

import StaticCheck.Format
import Data.Map as M

checkInterfaces :: SATProgramFormat -> Either String ISATProgramFormat
checkInterfaces (SATSIT structs interfaceList types) = do
    map <- makeMap interfaceList
    return $ ISATSIT structs map types

makeMap :: [FInterface] -> Either String (M.Map String FInterface)
makeMap [] = return empty
makeMap (i@(FInterfaceB name _):xs) = do
    map <- makeMap xs
    if M.member name map 
        then fail "UUU TODO" 
        else return $ M.insert name i map
makeMap (i@(FInterfaceI name _ _):xs) = do
    map <- makeMap xs
    if M.member name map
        then fail "UUU TODO"
        else return $ M.insert name i map