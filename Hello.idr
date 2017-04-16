module Main

allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

main : IO ()
main = putStrLn (cast 'x')
