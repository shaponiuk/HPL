module Messages.Messages where

printNoFile :: IO ()
printNoFile = putStrLn "TODO"

printTooManyFiles :: IO ()
printTooManyFiles = putStrLn "TODO"

printWarnings :: [String] -> IO ()
printWarnings [] = return ()
printWarnings (w:ws) = do
    putStrLn w
    printWarnings ws