-- "I say hello %d times" => Lit ("I say hello " (Number (Lit " times" End)))
data Format =
    Number Format |
    Str Format |
    Lit String Format |
    End

-- Type of `printf` for the given formatting style.
-- Examples: PrintfType "I say hello %d times" = Int -> String
total
PrintfType : Format -> Type
PrintfType (Number fmt) = Int -> PrintfType fmt
PrintfType (Str fmt) = String -> PrintfType fmt
PrintfType (Lit lit fmt) = PrintfType fmt
PrintfType End = String -- output type

total
printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt') acc = \int => printfFmt fmt' (acc ++ cast int)
printfFmt (Str fmt') acc = \str => printfFmt fmt' (acc ++ str)
printfFmt (Lit lit fmt') acc = printfFmt fmt' (acc ++ lit)
printfFmt End acc = acc

total
toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: rest) = Number (toFormat rest)
toFormat ('%' :: 's' :: rest) = Str (toFormat rest)
toFormat (x :: xs) =
    case toFormat xs of
        Lit s fmt => Lit (strCons x s) fmt
        fmt => Lit (cast x) fmt

total
printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""
