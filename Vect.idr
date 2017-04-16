import Data.Vect

fourInts : Vect 4 Int
fourInts = [1, 2, 3, 4]

total allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

insert : Ord elem =>
         (x : elem) ->
         (sortedXs : Vect len elem) ->
         Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = if x < y then x :: y :: xs
                              else y :: insert x xs

||| These doc comments are weird
total insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let sortedXs = insSort xs in
                        insert x sortedXs
