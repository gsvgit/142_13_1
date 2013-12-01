let stringHash (str: string) = 
    let a = ref 0
    if str.[str.Length - 2] <> ' ' 
    then a := !a + 1
    for i in 1..str.Length - 1 do
        if str.[i] = ' ' && str.[i - 1] <> ' '
        then a := !a + 1
    printfn "There are %A words in this string" !a
stringHash "              AC\DC                       "
stringHash "      Back in black             "
stringHash "  Rock'n'roll train"
stringHash "Highway to hell"