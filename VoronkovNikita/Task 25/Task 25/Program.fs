let stringHash (str: string) = 
    let a = ref 0
    if str.Length = 1 && str.[0] = ' ' || str.Length  = 0
    then a := 0
    elif (str.[0] <> ' ' && (str.Length = 1 || str.Length = 2)) || (str.[0] <> ' ' && str.[1] = ' ' && str.Length = 2)
    then a := 1
    else
        if str.[str.Length - 2] = ' ' && str.[str.Length - 1] <> ' '
        then a := !a + 1
        for i in 1..str.Length - 1 do
            if str.[i] = ' ' && str.[i - 1] <> ' '
            then a := !a + 1
    if str.[str.Length - 2] <> ' ' && str.[str.Length - 1] <> ' '
    then !a + 1
    else !a
stringHash "              AC\DC                       " |> printfn "%A"
stringHash "      Back in black             " |> printfn "%A"
stringHash "  Rock'n'roll train" |> printfn "%A"
stringHash "1 2 3" |> printfn "%A"