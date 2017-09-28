-- The type of `adder` when the number of additional arguments to `adder` is `n`.
--
-- adder 0 : Int -> Int
-- adder 1 : Int -> Int -> Int
total
AdderType : (n : Nat) -> Type
AdderType Z = Int
AdderType (S k) = Int -> AdderType k

total
adder : (numArgs : Nat) -> (acc : Int) -> AdderType numArgs
adder Z acc = acc
adder (S k) acc = \x => adder k (acc + x)

-- Version of adder which allows 0 real arguments
total
altAdder : (numArgs : Nat) -> AdderType numArgs
altAdder Z = 0
altAdder (S k) = \x => adder k x
