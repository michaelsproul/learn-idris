module Main

import Data.Vect

readVect : IO (n ** Vect n String)
readVect = do
    line <- getLine
    if line == "" then
        pure (_ ** [])
    else do
        (_ ** rest) <- readVect
        pure (_ ** line :: rest)

printVect : Show a => (n ** Vect n a) -> IO ()
printVect (len ** v) = putStrLn (show v ++ " (length " ++ show len ++ ")")

zipInputs : IO ()
zipInputs = do
    putStrLn "Enter the first vector (blank line to end):"
    (l1 ** v1) <- readVect
    putStrLn "Enter the second vector (blank line to end):"
    (l2 ** v2) <- readVect
    case exactLength l1 v2 of
        Nothing => putStrLn "Error: can't zip, vectors are different lengths!"
        Just v2' => printLn (zip v1 v2')

main : IO ()
main = zipInputs
