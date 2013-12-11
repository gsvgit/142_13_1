let strHash (str: string) = 
    let hash = ref 1
    if (str.Length = 0 || str.Length = 1 && str.[0] = ' ')  
    then hash := !hash - 1
    else
        for i in 0 .. 2 .. str.Length - 1 do
            hash := (!hash * int (str.[i]) - str.Length * i)
    !hash
printfn "%A" (strHash "abcdefg")
printfn "%A" (strHash "5r")
printfn "%A" (strHash "john16")
printfn "%A" (strHash "")
