module Main

import Data.Vect

data DataStore : Type where
    MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData n _) = n

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

addToDataStore : DataStore -> String -> DataStore
addToDataStore store newItem = MkData _ ((items store) ++ [newItem])

data Command =
    Add String |
    Get Integer |
    Quit

parseCommand : String -> String -> Maybe Command
parseCommand "add" s = (Just . Add) s
parseCommand "get" s = map Get maybeIdx
    where
        maybeIdx = if all isDigit (unpack s) then Just (cast s) else Nothing
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : String -> Maybe Command
parse str =
    let (cmd, args) = span (/= ' ') str in
    parseCommand cmd (ltrim args)

getEntry : Integer -> DataStore -> Maybe (String, DataStore)
getEntry x store =
    let storeItems = items store in
    let maybeIdx = integerToFin x (size store) in
    case maybeIdx of
        Nothing => Just ("Out of range\n", store)
        Just idx => Just (index idx storeItems ++ "\n", store)

processCommand : (cmd : Command) -> (store : DataStore) -> Maybe (String, DataStore)
processCommand (Add s) store = Just ("ID " ++ show (size store) ++ "\n", addToDataStore store s)
processCommand (Get x) store = getEntry x store
processCommand Quit _ = Nothing

onInput : DataStore -> String -> Maybe (String, DataStore)
onInput store input =
    case parse input of
        Nothing => Just ("error: invalid command\n", store)
        Just cmd => processCommand cmd store

main : IO ()
main = replWith emptyStore "datastore > " onInput
    where emptyStore = MkData _ []
