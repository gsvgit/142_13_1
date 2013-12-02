let stringHash (str: string) = 
    let a = ref 0
    if str.[str.Length - 2] <> ' ' 
    then a := !a + 1
    for i in 1..str.Length - 1 do
        if str.[i] = ' ' && str.[i - 1] <> ' '
        then a := !a + 1
    !a
stringHash "              AC\DC                       " |> printfn "%A"
stringHash "      Back in black             " |> printfn "%A"
stringHash "  Rock'n'roll train" |> printfn "%A"
stringHash "Highway to hell" |> printfn "%A"